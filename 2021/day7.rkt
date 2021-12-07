#lang racket

(define (read-crabs path)
  ((compose
    ((curry map) string->number)
    (lambda (s) (string-split s ","))
    file->string) path))

(define (arithmetic-sum n)
  (/ (* n (add1 n)) 2))

(define (min-fuel crabs [cost (lambda (x) x)])
  (for/fold ([min-fuel (inexact->exact 1e12)])
            ([target (in-range 0 (add1 (apply max crabs)))])
    (min min-fuel
         (for/sum ([pos crabs])
           (cost (abs (- target pos)))))))

(printf "Part-1: ~a\n" (min-fuel (read-crabs "day7-input.txt")))
(printf "Part-2: ~a\n" (min-fuel (read-crabs "day7-input.txt") arithmetic-sum))