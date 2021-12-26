#lang racket

(require "memo.rkt")

(define (string->step s)
  (define (num-range f t)
    (cons (string->number f) (string->number t)))
  (match-define (list _ st x1 x2 y1 y2 z1 z2)
    (regexp-match #px"(on|off) x=(-?\\d+)\\.\\.(-?\\d+),y=(-?\\d+)\\.\\.(-?\\d+),z=(-?\\d+)\\.\\.(-?\\d+)" s))
  (list (string->symbol st) (num-range x1 x2) (num-range y1 y2) (num-range z1 z2)))

(define (read-steps path)
  (map string->step (file->lines path)))

(define (axis-values steps p)
  (for/fold ([v (seteqv)]
             #:result
             (let ([vec (list->vector (set->list v))])
               (vector-sort! vec <)
               vec))
            ([step steps])
    (match-define (cons a1 a2) (p step))
    (set-add (set-add v (sub1 a1)) a2)))

(define (x-within? x step)
  (match-define (list _ (cons x1 x2) _ _) step)
  (and (>= x x1) (<= x x2)))

(define (vector-ceil vt v)
  (let find ([lo 0]
             [hi (sub1 (vector-length vt))])
    (define m (+ lo (remainder (- hi lo) 2)))
    (cond
      [(> lo hi) lo]
      [(< (vector-ref vt m) v) (find (+ m 1) hi)]
      [else (find lo (- m 1))])))

(define (matrix-set vt r c v)
  (vector-set! (vector-ref vt r) c v))

(define/memo (count-layer steps)
  (define y-axis (axis-values steps third))
  (define z-axis (axis-values steps fourth))
  (define layer
    (build-vector
     (vector-length y-axis)
     (λ (_) (make-vector
             (vector-length z-axis) 0))))
  (for ([step steps])
    (match-define (list st _ (cons y1 y2) (cons z1 z2)) step)
    (for* ([r (in-range (vector-ceil y-axis y1) (add1 (vector-ceil y-axis y2)))]
           [c (in-range (vector-ceil z-axis z1) (add1 (vector-ceil z-axis z2)))])
      (matrix-set layer r c (if (eq? st 'on) 1 0))))
  (for/sum ([(row r) (in-indexed layer)])
    (for/sum ([(cell c) (in-indexed row)]
              #:when (= cell 1))
      (define h (- (vector-ref y-axis r) (vector-ref y-axis (sub1 r))))
      (define w (- (vector-ref z-axis c) (vector-ref z-axis (sub1 c))))
      (* h w))))

(define (count-on steps)
  (define x-axis (vector->list (axis-values steps second)))
  (define x-min (argmin identity x-axis))
  (define x-max (argmax identity x-axis))
  (define seen (make-hash))
  (for/sum ([x (in-range x-min (add1 x-max))])
    (count-layer
     (for/list ([step steps]
                #:when (x-within? x step))
       step))))

(define (part-1 steps)
  (count-on
   (for/list ([step steps])
     (match-define (list s (cons x1 x2) (cons y1 y2) (cons z1 z2)) step)
     (list s (cons (max -50 x1) (min 50 x2))
           (cons (max -50 y1) (min 50 y2))
           (cons (max -50 z1) (min 50 z2))))))

(printf "Part-1: ~a\n" (part-1 (read-steps "day22-input.txt")))

(printf "Part-2: ~a\n" (count-on (read-steps "day22-input.txt")))

(module+ test
  (require rackunit)
  (check-equal? (vector-ceil #(0 10 20 30) 10) 1)
  (check-equal? (vector-ceil #(0 10 20 30) 15) 2)
  (check-equal? (vector-ceil #(0 10 20 30) 31) 4)
  (check-equal? (part-1 (read-steps "day22-test-short.txt")) 590784)
  (check-equal? (count-on (read-steps "day22-test-long.txt")) 2758514936282235))