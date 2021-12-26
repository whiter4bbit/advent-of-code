#lang racket

(require "memo.rkt")

(define (read-timers path)
  (map string->number (string-split (file->string path) ",")))

(define (count-fish timers [deadline 80])
  (define/memo (count timer day)
    (define split-day (+ timer day 1))
    (if (> split-day deadline)
        1
        (+ (count 8 split-day) (count 6 split-day))))
  (for/sum ([timer timers])
    (count timer 0)))

(printf "Part-1: ~a\n" (count-fish (read-timers "day6-input.txt") 80))
(printf "Part-2: ~a\n" (count-fish (read-timers "day6-input.txt") 256))