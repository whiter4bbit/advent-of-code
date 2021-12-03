#lang racket

(define (majority nums i)
  (for/fold ([balance 0]
             #:result (if (< balance 0) #\0 #\1))
            ([num nums])
    (+ balance (if (equal? (string-ref num i) #\1) 1 -1))))

(define (bin->integer s) (string->number s 2))

(define (part-1 nums)
  (let* ([bits (string-length (car nums))]
         [gamma (bin->integer (build-string bits ((curry majority) nums)))])
    (* gamma (bitwise-xor gamma (sub1 (arithmetic-shift 1 bits))))))

(printf "Part-1: ~a\n" (part-1 (file->lines "day3-input.txt")))

(define (part-2 nums)
  (define (rating i nums compare)
    (define (keep-with-bit bit)
      (for/list ([num nums]
                 #:when (compare (string-ref num i) bit))
        num))
    (cond
      [(empty? (cdr nums)) (bin->integer (car nums))]
      [else (rating (add1 i) (keep-with-bit (majority nums i)) compare)]))
  (* (rating 0 nums equal?) (rating 0 nums (lambda (a b) (not (equal? a b))))))

(printf "Part-2: ~a\n" (part-2 (file->lines "day3-input.txt")))