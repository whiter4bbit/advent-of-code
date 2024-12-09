#lang racket

(define mul-px #px"mul\\((\\d{1,3})\\,(\\d{1,3})\\)")

(define mul-do-px #px"mul\\((\\d{1,3})\\,(\\d{1,3})\\)|do\\(\\)|don\\'t\\(\\)")

(define (eval-mul expr)
  (let* ([args (regexp-match mul-px expr)]) (for/product ([arg (cdr args)]) (string->number arg))))

(define (part-1 path)
  (let* ([prog (file->string path)]
         [exprs (regexp-match* mul-px prog)])
    (for/sum ([expr exprs]) (eval-mul expr))))

(define (part-2 path)
  (let* ([prog (file->string path)]
         [exprs (regexp-match* mul-do-px prog)])
    (for/fold ([enabled? 1]
               [sum 0]
               #:result sum)
              ([expr exprs])
      (cond
        [(equal? expr "do()") (values 1 sum)]
        [(equal? expr "don't()") (values 0 sum)]
        [else (values enabled? (+ sum (* (eval-mul expr) enabled?)))]))))

(printf "Part-1: ~a\n" (part-1 "day03-input.txt"))
(printf "Part-2: ~a\n" (part-2 "day03-input.txt"))
