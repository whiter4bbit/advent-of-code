#lang racket

(require racket/match)

(define (read-chitons path)
  (for/fold ([map* (hash)])
            ([(row i) (in-indexed (file->lines path))])
    (for/fold ([map* map*])
              ([(cell j) (in-indexed row)])
      (hash-set map* (cons i j) (- (char->integer cell) (char->integer #\0))))))

(define chitons (read-chitons "day15-input.txt"))
(define inf-cost 1000000000)

(define (neighbors pos)
  (for/list ([offset '((-1 . 0) (1 . 0) (0 . -1) (0 . 1))])
    (cons (+ (car pos) (car offset)) (+ (cdr pos) (cdr offset)))))

(define (lowest-risk target-pos chiton-cost)
  (define costs (make-hash))
  (hash-set! costs '(-1 . 0) (- (chiton-cost '(0 . 0))))
  (for* ([row (in-range (add1 (car target-pos)))]
         [col (in-range (add1 (cdr target-pos)))])
    (define left-cost (hash-ref! costs (cons row (sub1 col)) inf-cost))
    (define top-cost (hash-ref! costs (cons (sub1 row) col) inf-cost))
    (hash-set! costs (cons row col) (+ (min left-cost top-cost) (chiton-cost (cons row col)))))
  (hash-ref! costs target-pos #f))

(define (single-tile-bottom-corner chitons)
  (cons (car (argmax car (hash-keys chitons)))
        (cdr (argmax cdr (hash-keys chitons)))))

(define (single-tile-cost chitons)
  (lambda (pos) (hash-ref chitons pos inf-cost)))

(printf "Part-1: ~a\n" (lowest-risk (single-tile-bottom-corner chitons) (single-tile-cost chitons)))

(define (multi-tile-bottom-corner chitons)
  (match-define (cons row col) (single-tile-bottom-corner chitons))
  (cons (sub1 (* (add1 row) 5)) (sub1 (* (add1 col) 5))))

(define (multi-tile-cost chitons)
  (define single-tile-size (add1 (car (single-tile-bottom-corner chitons))))
  (define multi-tile-size (* single-tile-size 5))
  (lambda (pos)
    (match-define (cons row col) pos)
    (cond
      [(or (< row 0) (< col 0) (>= row multi-tile-size) (>= col multi-tile-size)) inf-cost]
      [else
       (define row-factor (quotient row single-tile-size))
       (define col-factor (quotient col single-tile-size))
       (define cost* ((single-tile-cost chitons) (cons (remainder row single-tile-size)
                                                       (remainder col single-tile-size))))
       (add1 (remainder (sub1 (+ cost* (+ row-factor col-factor))) 9))])))

(printf "Part-2: ~a\n" (lowest-risk (multi-tile-bottom-corner chitons) (multi-tile-cost chitons)))