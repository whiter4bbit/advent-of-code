#lang racket

(define (read-polymer path)
  (string->list (first (file->lines path))))

(define (read-rules path)
  (for/hash ([raw (drop (file->lines path) 2)])
    (values (cons (string-ref raw 0) (string-ref raw 1)) (string-ref raw 6))))

(define (hash-add h k b)
  (hash-update h k (lambda (v) (+ v b)) 0))

(define (count-elements polymer rules times)
  (define (populate-elements elements pairs)
    (for/fold ([elements elements])
              ([(pair elem) rules])
      (hash-add elements elem (hash-ref pairs pair 0))))
  (define (populate-pairs pairs)
    (for/fold ([next-pairs (hash)])
              ([(pair elem) rules])
      (for/fold ([next-pairs next-pairs])
                ([next-pair (list (cons (car pair) elem) (cons elem (cdr pair)))])
        (hash-add next-pairs next-pair (hash-ref pairs pair 0)))))
  (for/fold ([pairs (for/fold ([pairs (hash)])
                              ([p polymer]
                               [n (cdr polymer)])
                      (hash-add pairs (cons p n) 1))]
             [elements (for/fold ([elements (hash)])
                                 ([elem polymer])
                         (hash-add elements elem 1))]
             #:result elements)
            ([_ (in-range times)])
    (values (populate-pairs pairs) (populate-elements elements pairs))))

(define (apply/diff polymer rules times)
  (let ([elements (hash-values (count-elements polymer rules times))])
    (- (apply max elements)
       (apply min elements))))

(printf "Part-1: ~a\n" (apply/diff (read-polymer "day14-input.txt") (read-rules "day14-input.txt") 10))
(printf "Part-2: ~a\n" (apply/diff (read-polymer "day14-input.txt") (read-rules "day14-input.txt") 40))