#lang racket

(define (read-observations path)
  (for/list ([raw (file->lines path)])
    (let* ([tokens (string-split raw " | ")]
           [patterns (map string->list (string-split (first tokens)))]
           [output (map string->list (string-split (second tokens)))])
      (cons patterns output))))

(define bitmask->digit '#hasheq((#x77 . 0) (#x24 . 1) (#x5d . 2) (#x6d . 3) (#x2e . 4) (#x6b . 5) (#x7b . 6) (#x25 . 7) (#x7f . 8) (#x6f . 9)))

(define (signals->digit key signals)
  (for/fold ([bitmask 0]
             #:result (hash-ref bitmask->digit bitmask #f))
            ([signal signals])
    (bitwise-ior bitmask (arithmetic-shift 1 (index-of key signal)))))

(define (key-valid? key patterns)
  (for/fold ([digits 0]
             #:result (= digits 1023))
            ([signals patterns] #:break (not (signals->digit key signals)))
    (bitwise-ior digits (arithmetic-shift 1 (signals->digit key signals)))))

(define (decode-outputs observations)
  (for/list ([observation observations])
    (for/first ([key (in-permutations '(#\a #\b #\c #\d #\e #\f #\g))]
                #:when (key-valid? key (car observation)))
      (for/list ([signals (cdr observation)])
        (signals->digit key signals)))))

(define observations-output (decode-outputs (read-observations "day8-input.txt")))

(define (part-1 observations-output)
  (for/sum ([digits observations-output])
    (for/sum ([digit digits]
              #:when (member digit '(1 4 7 8)))
      1)))

(printf "Part-1: ~a\n" (part-1 observations-output))

(define (part-2 observations-output)
  (for/sum ([digits observations-output])
    (+ (* 1000 (first digits))
       (* 100  (second digits))
       (* 10   (third digits))
       (* 1    (fourth digits)))))

(printf "Part-2: ~a\n" (part-2 observations-output))