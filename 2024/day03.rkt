#lang racket

(define mul-px #px"mul\\((\\d{1,3})\\,(\\d{1,3})\\)")

(define mul-do-px #px"mul\\((\\d{1,3})\\,(\\d{1,3})\\)|do\\(\\)|don\\'t\\(\\)")

(define (eval-mul mul)
  (let* ([args (regexp-match mul-px mul)]) (for/product ([arg (cdr args)]) (string->number arg))))

(define (part-1 path)
  (let* ([prog (string-join (file->lines path) "")]
         [matches (regexp-match* mul-px prog)])
    (for/sum ([mul matches]) (eval-mul mul))))

(define (part-2 path)
  (let* ([prog (string-join (file->lines path) "")]
         [matches (regexp-match* mul-do-px prog)])
    (for/fold ([enabled? #t]
               [sum 0]
               #:result sum)
              ([expr matches])
      (cond
        [(equal? expr "do()") (values #t sum)]
        [(equal? expr "don't()") (values #f sum)]
        [else
         (values enabled?
                 (if enabled?
                     (+ sum (eval-mul expr))
                     sum))]))))

(printf "Part-1: ~a\n" (part-1 "day03-input.txt"))
(printf "Part-2: ~a\n" (part-2 "day03-input.txt"))