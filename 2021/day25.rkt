#lang racket

(define (read-grid path)
  (define lines (file->lines path))
  (define rows (length lines))
  (define cols (string-length (first lines)))
  (define grid (build-vector rows (λ (_) (make-vector cols #f))))
  (for ([(row i) (in-indexed lines)])
    (for ([(cell j) (in-indexed row)]
          #:when (or (eq? cell #\>) (eq? cell #\v)))
      (vector-set! (vector-ref grid i) j cell)))
  grid)

(define (grid-dim grid)
  (values (vector-length grid)
          (vector-length (vector-ref grid 0))))

(define (grid-ref grid pos)
  (vector-ref (vector-ref grid (car pos)) (cdr pos)))

(define (grid-set! grid pos v)
  (vector-set! (vector-ref grid (car pos)) (cdr pos) v))

(define (neighbor/pos grid pos)
  (define-values [rows cols] (grid-dim grid))
  (match-define (cons r c) pos)
  (if (eq? (grid-ref grid pos) #\>)
      (cons r (remainder (add1 c) cols))
      (cons (remainder (add1 r) rows) c)))

(define (neighbor/val grid pos)
  (grid-ref grid (neighbor/pos grid pos)))

(define (move! grid type)
  (define-values [rows cols] (grid-dim grid))
  (define moves
    (for*/fold ([pos* empty])
               ([r (in-range rows)]
                [c (in-range cols)]
                #:when (and (eq? (grid-ref grid (cons r c)) type)
                            (not (neighbor/val grid (cons r c)))))
      (cons (cons r c) pos*)))
  (for ([pos moves])
    (grid-set! grid (neighbor/pos grid pos) type)
    (grid-set! grid pos #f))
  (not (empty? moves)))

(define (move*! grid)
  (define left (move! grid #\>))
  (define down (move! grid #\v))
  (or left down))

(define (show grid)
  (define-values [rows cols] (grid-dim grid))
  (for ([r (in-range rows)])
    (for ([c (in-range cols)])
      (printf "~a" (or (grid-ref grid (cons r c)) #\.)))
    (printf "\n")))

(define (part-1 grid)
  (for/last ([i (in-naturals)]
             #:break (not (move*! grid)))
    (+ i 2)))

(printf "Part-1: ~a\n" (part-1 (read-grid "day25-input.txt")))

(module+ test
  (require rackunit)
  (check-equal? (part-1 (read-grid "day25-test.txt")) 58))