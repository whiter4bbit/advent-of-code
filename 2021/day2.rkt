#lang racket

(require racket/match)

(define (read-course path)
    (for/list ([entry (file->lines path)])
        (define-values [command units] (apply values (string-split entry)))
        (cons (string->symbol command) (string->number units))))

(define (part-1 course)
    (for/fold ([horizontal 0]
               [depth 0]
               #:result (* depth horizontal))
              ([step course])
              (match step
                [(cons 'forward units) (values (+ units horizontal) depth)]
                [(cons 'up units) (values horizontal (- depth units))]
                [(cons 'down units) (values horizontal (+ depth units))])))

(printf "Part-1: ~a\n" (part-1 (read-course "day2-input.txt")))

(define (part-2 course)
    (for/fold ([horizontal 0]
               [depth 0]
               [aim 0]
               #:result (* depth horizontal))
              ([step course])
              (match step
                [(cons 'forward units) (values (+ units horizontal) (+ depth (* aim units)) aim)]
                [(cons 'up units) (values horizontal depth (- aim units))]
                [(cons 'down units) (values horizontal depth (+ aim units))])))

(printf "Part-2: ~a\n" (part-2 (read-course "day2-input.txt")))