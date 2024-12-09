#lang racket

(define (file->pairs path)
  (map (lambda (line) (map string->number (string-split line "   "))) (file->lines path)))

(define (part-1 path)
  (let* ([pairs (file->pairs path)]
         [left-numbers (sort (map first pairs) <)]
         [right-numbers (sort (map second pairs) <)]
         [number-pairs (map cons left-numbers right-numbers)])
    (for/sum ([pair number-pairs]) (abs (- (car pair) (cdr pair))))))

(define (count-numbers numbers)
  (for/fold ([counts (hash)]) ([number numbers])
    (values (hash-update counts number add1 0))))

(define (part-2 path)
  (let* ([pairs (file->pairs path)]
         [right-numbers-count (count-numbers (map second pairs))])
    (for/sum ([left-number (map first pairs)])
             (* left-number (hash-ref right-numbers-count left-number 0)))))

(printf "Part-1: ~a\n" (part-1 "day01-input.txt"))
(printf "Part-2: ~a\n" (part-2 "day01-input.txt"))
