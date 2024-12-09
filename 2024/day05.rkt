#lang racket

(define (make-rules-hash rule-pairs)
  (for*/fold ([rules (make-immutable-hash)]) ([pair rule-pairs])
    (values (hash-update rules (car pair) (λ (r) (set-add r (cadr pair))) (set)))))

(define (file->input path)
  (let* ([str (file->string path)]
         [parts (string-split str "\n\n")]
         [rule-pairs (map (λ (e) (string-split e "|")) (string-split (car parts) "\n"))]
         [rules (make-rules-hash rule-pairs)]
         [updates (map (λ (e) (string-split e ",")) (string-split (cadr parts) "\n"))])
    (list rules rule-pairs updates)))

(define (violates? page rules seen)
  (not (set-empty? (set-intersect seen (hash-ref rules page (set))))))

(define (right-order? update rules)
  (for*/fold ([seen (set)]
              [right? #t]
              #:result right?)
             ([page update])
    (values (set-add seen page)
            (cond
              [(violates? page rules seen) (and right? #f)]
              [else (and right? #t)]))))

(define (list-mid lst)
  (string->number (list-ref lst (quotient (length lst) 2))))

(define (part-1 path)
  (let* ([input (file->input path)]
         [rules (car input)]
         [updates (caddr input)])
    (for/sum ([update updates] #:when (right-order? update rules)) (list-mid update))))

(printf "Part-1: ~a\n" (part-1 "day05-input.txt"))

(define (rules-has-pair? rules-hash lo hi)
  (set-member? (hash-ref rules-hash lo (set)) hi))

(define (list-swap lst i-pos j-pos)
  (let ([i-val (list-ref lst i-pos)]
        [j-val (list-ref lst j-pos)])
    (list-set (list-set lst j-pos i-val) i-pos j-val)))

(define (fix-update update rules-hash)
  (for*/fold ([fixed update]
              #:result fixed)
             ([i (in-range 0 (length update))]
              [j (in-range (+ i 1) (length update))])
    (cond
      [(rules-has-pair? rules-hash (list-ref fixed j) (list-ref fixed i))
       (values (list-swap fixed i j))]
      [else (values fixed)])))

(define (part-2 path)
  (let* ([input (file->input path)]
         [rules-hash (car input)]
         [rule-pairs (cadr input)]
         [updates (caddr input)])
    (for/sum ([update updates] #:when (not (right-order? update rules-hash)))
             (let ([fixed (fix-update update rules-hash)])
               (list-mid (fix-update update rules-hash))))))

(printf "Part-2: ~a\n" (part-2 "day05-input.txt"))
