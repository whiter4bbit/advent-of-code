#lang racket

(require racket/match)

(define (read-map path)
  (for/fold ([map (hash)])
            ([raw (file->lines path)])
    (match-define (list u v) (string-split raw "-"))
    (let* ([dir (hash-update map u ((curry cons) v) empty)]
           [rev (hash-update dir v ((curry cons) u) empty)])
      rev)))

(define (part-1 map)
  (define (count-paths seen start)
    (cond
      [(equal? "end" start) 1]
      [(and (char-lower-case? (string-ref start 0)) (set-member? seen start)) 0]
      [else
       (for/sum ([nei (hash-ref map start empty)])
         (count-paths (set-add seen start) nei))]))
  (count-paths (set) "start"))

(printf "Part-1: ~a\n" (part-1 (read-map "day12-input.txt")))

(define (part-2 map)
  (define (count-paths seen-once seen-twice start)
    (cond
      [(equal? "end" start) 1]
      [(and (equal? start "start") (set-member? seen-once start)) 0]
      [(and (char-lower-case? (string-ref start 0))
            (set-member? seen-once start)
            seen-twice) 0]
      [else
       (let ([next-seen-once
              (cond
                [(set-member? seen-once start) seen-once]
                [(char-lower-case? (string-ref start 0)) (set-add seen-once start)]
                [else seen-once])]
             [next-seen-twice
              (cond
                [(set-member? seen-once start) start]
                [else seen-twice])])
         (for/sum ([nei (hash-ref map start empty)])
           (count-paths next-seen-once next-seen-twice nei)))]))
  (count-paths (set) #f "start"))

(printf "Part-2: ~a\n" (part-2 (read-map "day12-input.txt")))