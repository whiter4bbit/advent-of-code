#lang racket

(define (majority nums i)
    (let ([balance (for/sum ([num nums]) (if (equal? (list-ref num i) #\1) 1 -1))])
        (if (< balance 0) #\0 #\1)))

(define (part-1 nums)
    (define bits (length (first nums)))
    (define gamma (string->number (build-string bits (lambda (i) (majority nums i))) 2))
    (define epsilon (bitwise-xor gamma (sub1 (arithmetic-shift 1 bits))))
    (* gamma epsilon))

(printf "Part-1: ~a\n" (part-1 (map string->list (file->lines "day3-input.txt"))))

(define (part-2 nums)
    (define (rating i nums compare)
        (cond
            [(empty? (cdr nums)) (string->number (list->string (car nums)) 2)]
            [else 
                (define bit (majority nums i))
                (rating
                    (add1 i)
                    (for/list ([num nums] 
                               #:when (compare (list-ref num i) bit))
                               num)
                    compare)]))
    (* (rating 0 nums equal?) (rating 0 nums (lambda (a b) (not (equal? a b))))))

(printf "Part-2: ~a\n" (part-2 (map string->list (file->lines "day3-input.txt"))))