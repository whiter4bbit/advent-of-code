#lang racket

(define (read-monad path)
  (for/list ([raw (file->lines path)])
    (for/list ([arg (string-split raw)])
      (define num (string->number arg))
      (or num (string->symbol arg)))))

(define (run monad input)
  (for/fold ([state (hasheq)]
             [input input]
             #:result (hash-ref state 'z))
            ([instr monad])
    (define (val r)
      (if (number? r) r (hash-ref state r 0)))
    (match instr
      [(list 'inp r) (values (hash-set state r (car input)) (cdr input))]
      [(list 'add a b) (values (hash-set state a (+ (val a) (val b))) input)]
      [(list 'mul a b) (values (hash-set state a (* (val a) (val b))) input)]
      [(list 'div a b) (values (hash-set state a (quotient (val a) (val b))) input)]
      [(list 'mod a b) (values (hash-set state a (remainder (val a) (val b))) input)]
      [(list 'eql a b) #:when (= (val a) (val b)) (values (hash-set state a 1) input)]
      [(list 'eql a b) (values (hash-set state a 0) input)])))

(define (find-accepted monad y-range)
  (define vars
    (for/list ([i (in-range 14)])
      (define routine (drop monad (* i 18)))
      (match-define (list 'div 'z div-z) (list-ref routine 4))
      (match-define (list 'add 'x add-x) (list-ref routine 5))
      (match-define (list 'add 'y add-y) (list-ref routine 15))
      (list i div-z add-x add-y)))
  (define input (make-vector 14 1))
  (let build ([stack empty]
              [vars vars])
    (match vars
      [(list) #f]
      [(cons (list _ _ add-x _ ) tail)
       #:when (> add-x 0)
       (build (cons (car vars) stack) tail)]
      [else
       (match-define (list i _ _ add-y) (car stack))
       (match-define (list j _ add-x _) (car vars))
       (define (x y)
         (+ y add-y add-x))
       (for/first ([y y-range]
                   #:when (and (> (x y) 0) (< (x y) 10)))
         (vector-set! input i y)
         (vector-set! input j (x y)))
       (build (cdr stack) (cdr vars))]))
  (if (= (run monad (vector->list input)) 0)
      (string-join (map number->string (vector->list input)) "")
      #f))

(define monad (read-monad "day24-input.txt"))

(printf "Part-1: ~a\n" (find-accepted monad (in-range 9 0 -1)))
(printf "Part-2: ~a\n" (find-accepted monad (in-range 1 10)))