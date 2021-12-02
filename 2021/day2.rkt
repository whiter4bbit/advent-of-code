#lang racket

(require racket/match)

(define (read-course path)
    (for/list ([entry (file->lines path)])
        (define-values [command units] (apply values (string-split entry)))
        (cons (string->symbol command) (string->number units))))

(define (part-1 course)
    (for/fold ([position 0+0i]
               #:result (* (real-part position) (imag-part position)))
              ([step course])
              (match step
                [(cons 'forward units) (+ position (* units 0+1i))]
                [(cons 'up units) (- position units)]
                [(cons 'down units) (+ position units)])))
                
(printf "Part-1: ~a\n" (part-1 (read-course "day2-input.txt")))

(define (part-2 course)
    (for/fold ([position 0+0i]
               [aim 0]
               #:result (* (real-part position) (imag-part position)))
              ([step course])
              (match step
                [(cons 'forward units) (values (+ position (* units 0+1i) (* units aim)) aim)]
                [(cons 'up units) (values position (- aim units))]
                [(cons 'down units) (values position (+ aim units))])))

(printf "Part-2: ~a\n" (part-2 (read-course "day2-input.txt")))