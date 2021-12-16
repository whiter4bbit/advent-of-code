#lang racket

(struct bit-buffer (num pos) #:transparent #:mutable)

(define (string->bit-buffer s)
  (bit-buffer
   (string->number s 16)
   (sub1 (* 4 (string-length s)))))

(define (peek-bits bb n)
  (bitwise-and
   (arithmetic-shift (bit-buffer-num bb) (- (- (bit-buffer-pos bb) (sub1 n))))
   (sub1 (arithmetic-shift 1 n))))

(define (take-bits! bb n)
  (let ([peek (peek-bits bb n)])
    (set-bit-buffer-pos! bb (- (bit-buffer-pos bb) n))
    peek))

(define (take-bit-buffer! bb n)
  (bit-buffer (take-bits! bb n) (sub1 n)))

(define (bit-buffer-eof? bb)
  (< (bit-buffer-pos bb) 6))

(struct pkt-literal (version value) #:transparent)
(struct pkt-operator (version type packets) #:transparent)

(define (read-pkt-literal bb version)
  (let read ([value 0]
             [group (take-bits! bb 5)])
    (cond
      [(= (arithmetic-shift group -4) 1)
       (read
        (bitwise-ior (arithmetic-shift value 4) (bitwise-and group #xF))
        (take-bits! bb 5))]
      [else
       (pkt-literal
        version
        (bitwise-ior (arithmetic-shift value 4) group))])))

(define (read-pkt-operator bb v t)
  (pkt-operator
   v t
   (case (take-bits! bb 1)
     [(0) (read-pkts (take-bit-buffer! bb (take-bits! bb 15)))]
     [(1) (for/list ([i (in-range (take-bits! bb 11))])
            (read-pkt bb))])))

(define (read-pkts bb [current empty])
  (if (bit-buffer-eof? bb)
      (reverse current)
      (read-pkts bb (cons (read-pkt bb) current))))

(define (read-pkt bb)
  (match (cons (take-bits! bb 3) (take-bits! bb 3))
    [(cons v 4) (read-pkt-literal bb v)]
    [(cons v t) (read-pkt-operator bb v t)]))

(define (string->pkt s)
  (read-pkt (string->bit-buffer s)))

(define (part-1 pkt)
  (cond
    [(pkt-literal? pkt) (pkt-literal-version pkt)]
    [(pkt-operator? pkt)
     (+ (pkt-operator-version pkt)
        (apply + (map part-1 (pkt-operator-packets pkt))))]))

(printf "Part-1: ~a\n" (part-1 (string->pkt (file->string "day16-input.txt"))))

(define (eval-pkt pkt)
  (match pkt
    [(pkt-literal _ v) v]
    [(pkt-operator _ 0 args) (apply + (map eval-pkt args))]
    [(pkt-operator _ 1 args) (apply * (map eval-pkt args))]
    [(pkt-operator _ 2 args) (apply min (map eval-pkt args))]
    [(pkt-operator _ 3 args) (apply max (map eval-pkt args))]
    [(pkt-operator _ 5 (list f s)) #:when (> (eval-pkt f) (eval-pkt s)) 1]
    [(pkt-operator _ 6 (list f s)) #:when (< (eval-pkt f) (eval-pkt s)) 1]
    [(pkt-operator _ 7 (list f s)) #:when (equal? (eval-pkt f) (eval-pkt s)) 1]
    [else 0]))

(printf "Part-2: ~a\n" (eval-pkt (string->pkt (file->string "day16-input.txt"))))