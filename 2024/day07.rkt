#lang racket

(define (file->equations path)
  (let* ([lines (file->lines path)])
    (for/list ([line lines])
      (map string->number (regexp-split #px"\\: | " line)))))

(define (can-eval? goal curr nums)
  (if (null? nums)
      (equal? goal curr)
      (or (can-eval? goal (+ curr (car nums)) (cdr nums))
          (can-eval? goal (* curr (car nums)) (cdr nums)))))

(define (part-x eval-fn path)
  (for/sum ([equation (file->equations path)])
           (let* ([goal (car equation)]
                  [nums (cdr equation)])
             (if (eval-fn goal (car nums) (cdr nums)) goal 0))))

(printf "Part-1: ~a\n" (part-x can-eval? "day07-input.txt"))

(define (number-concat a b)
  (string->number (string-append (number->string a) (number->string b))))

(define (can-eval*? goal curr nums)
  (if (null? nums)
      (equal? goal curr)
      (or (can-eval*? goal (+ curr (car nums)) (cdr nums))
          (can-eval*? goal (* curr (car nums)) (cdr nums))
          (can-eval*? goal (number-concat curr (car nums)) (cdr nums)))))

(printf "Part-2: ~a\n" (part-x can-eval*? "day07-input.txt"))
