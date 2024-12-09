#lang racket

(define (foo)
  (define x 10)
  (set! x 20)
  (display x))

(foo)