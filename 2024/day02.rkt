#lang racket

(define (file->reports path)
  (map (lambda (line) (map string->number (string-split line " "))) (file->lines path)))

(define (make-diff lhs rhs)
  (if (or (null? rhs) (null? lhs))
      null
      (cons (- (car lhs) (car rhs)) (make-diff (cdr lhs) (cdr rhs)))))

(define (abs-between? lo hi)
  (lambda (num) (and (>= (abs num) lo) (<= (abs num) hi))))

(define (is-safe? report)
  (let* ([diff (make-diff report (cdr report))])
    (and (andmap (abs-between? 1 3) diff) (or (andmap positive? diff) (andmap negative? diff)))))

(define (remove-at lst at i)
  (if (null? lst)
      lst
      (if (eq? at i)
          (remove-at (cdr lst) remove-at (add1 i))
          (append (list (car lst)) (remove-at (cdr lst) at (add1 i))))))

(define (is-safe-any? report)
  (or (is-safe? report)
      (for/or ([i (in-range 0 (length report))])
        (is-safe? (remove-at report i 0)))))

(define (solve path check)
  (length (filter check (file->reports path))))

(printf "Part-1: ~a\n" (solve "day02-input.txt" is-safe?))
(printf "Part-2: ~a\n" (solve "day02-input.txt" is-safe-any?))
