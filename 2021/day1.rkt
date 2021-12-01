#lang racket

(define (count-incr depths)
    (for/sum ([p depths]
              [c (rest depths)]
              #:when (> c p)) 
              1))

(define (part-1 path)
    (define depths (map string->number (file->lines path)))
    (count-incr depths))

(printf "Part-1: ~a\n" (part-1 "day1-input.txt"))

(define (part-2 path)
    (define depths (map string->number (file->lines path)))
    (define window-sum
        (for/list ([a depths]
                   [b (cdr depths)]
                   [c (cddr depths)])
                  (+ a b c)))
    (count-incr window-sum))

(printf "Part-2: ~a\n" (part-2 "day1-input.txt"))