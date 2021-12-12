#lang racket

(define (!= a b)
  (not (= a b)))

(define (read-grid path)
  (for/vector ([row (file->lines path)])
    (for/vector ([c row])
      (- (char->integer c) (char->integer #\0)))))

(define (at-offset r c offset)
  (list (+ (first offset) r) (+ (second offset) c)))

(define (neighbors r c)
  (for*/list ([nr (in-range (sub1 r) (+ r 2))]
              [nc (in-range (sub1 c) (+ c 2))]
              #:when (and (or (!= r nr) (!= c nc)) (>= nr 0) (< nr 10) (>= nc 0) (< nc 10)))
    (list nr nc)))

(define (grid-ref grid r c)
  (vector-ref (vector-ref grid r) c))

(define (grid-incr! grid r c)
  (grid-set! grid r c (add1 (grid-ref grid r c)))
  (grid-ref grid r c))

(define (grid-set! grid r c v)
  (vector-set! (vector-ref grid r) c v))

(define (make-step! grid)
  (define flashed (mutable-set))
  (define (try-flash r c)
    (when (and (not (set-member? flashed (cons r c)))
               (> (grid-incr! grid r c) 9))
      (grid-set! grid r c 0)
      (set-add! flashed (cons r c))
      (for ([nei (neighbors r c)])
        (apply try-flash nei))))
  (for* ([r (in-range 10)]
         [c (in-range 10)])
    (try-flash r c))
  (set-count flashed))

(define (part-1 grid)
  (for/sum ([_ (in-range 100)])
    (make-step! grid)))

(printf "Part-1: ~a\n" (part-1 (read-grid "day11-input.txt")))

(define (part-2 grid)
  (for/or ([i (in-naturals)])
    (make-step! grid)
    (and
     (for*/and ([r (in-range 10)]
                [c (in-range 10)])
       (= (grid-ref grid r c) 0))
     (add1 i))))

(printf "Part-2: ~a\n" (part-2 (read-grid "day11-input.txt")))