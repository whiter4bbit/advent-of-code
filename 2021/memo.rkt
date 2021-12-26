#lang racket

(provide define/memo)

(define-syntax-rule (define/memo (fn arg ...) body ...)
  (begin
    (define seen (make-hash))
    (define (fn arg ...)
      (define hk** (list arg ...))
      (cond
        [(hash-has-key? seen hk**) (hash-ref! seen hk** #f)]
        [else
         (define res ((lambda (arg ...) body ...) arg ...))
         (hash-set! seen hk** res)
         res]))))