#lang racket

(require racket/match)

(define (read-dots path)
  (for/fold ([dots (set)])
            ([raw (file->lines path)]
             #:break (equal? "" raw))
    (match-define (list x y) (string-split raw ","))
    (set-add dots (cons (string->number x) (string->number y)))))

(define (read-folds path)
  (for/list ([raw (file->lines path)]
             #:when (string-prefix? raw "fold along "))
    (match-define (list axis u) (string-split (substring raw 11) "="))
    (cons (string->symbol axis) (string->number u))))

(define (apply-fold dots fold)
  (for/fold ([folded (set)])
            ([dot dots])
    (match-define (cons x y) dot)
    (match fold
      [(cons 'y v) #:when (> y v) (set-add folded (cons x (- v (- y v))))]
      [(cons 'x v) #:when (> x v) (set-add folded (cons (- v (- x v)) y))]
      [else (set-add folded dot)])))

(define (part-1 dots folds)
  (set-count (apply-fold dots (first folds))))

(printf "Part-1: ~a\n" (part-1 (read-dots "day13-input.txt") (read-folds "day13-input.txt")))

(define (part-2 dots folds)
  (define final-fold
    (for/fold ([dots dots])
              ([fold folds])
      (apply-fold dots fold)))
  (for ([y (in-range 7)])
    (for ([x (in-range 100)])
        (if (set-member? final-fold (cons x y)) (printf "#") (printf " ")))
    (printf "\n")))

(printf "Part-2:\n")
(part-2 (read-dots "day13-input.txt") (read-folds "day13-input.txt"))