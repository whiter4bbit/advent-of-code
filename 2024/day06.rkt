#lang racket

(define (list->ivector lst)
  (vector->immutable-vector (list->vector lst)))

(define (file->grid path)
  (let* ([lines (file->lines path)]
         [rows (list->ivector (map (λ (line) (list->ivector (string->list line))) lines))])
    rows))

(define (grid-rows grid)
  (vector-length grid))

(define (grid-cols grid)
  (vector-length (vector-ref grid 0)))

(define (grid-cell grid cell)
  (let* ([r (car cell)]
         [c (cadr cell)])
    (if (or (< r 0) (< c 0) (>= r (grid-rows grid)) (>= c (grid-cols grid)))
        null
        (vector-ref (vector-ref grid r) c))))

(define (grid-find-guard grid)
  (for*/first ([r (grid-rows grid)]
               [c (grid-cols grid)]
               #:when (equal? #\^ (grid-cell grid (list r c))))
    (list r c (grid-cell grid (list r c)))))

(define (guard-rotate guard)
  (let* ([r (car guard)]
         [c (cadr guard)]
         [f (caddr guard)])
    (cond
      [(equal? f #\^) (list r c #\>)]
      [(equal? f #\>) (list r c #\v)]
      [(equal? f #\v) (list r c #\<)]
      [else (list r c #\^)])))

(define (guard-pos guard)
  (list (car guard) (cadr guard)))

(define (guard-step guard)
  (let* ([r (car guard)]
         [c (cadr guard)]
         [f (caddr guard)])
    (cond
      [(equal? f #\^) (list (- r 1) c f)]
      [(equal? f #\>) (list r (+ c 1) f)]
      [(equal? f #\v) (list (+ r 1) c f)]
      [else (list r (- c 1) f)])))

(define (guard-peek grid guard)
  (let* ([step (guard-step guard)]
         [cell (guard-pos step)])
    (grid-cell grid cell)))

(define (guard-peek* grid obstacle guard)
  (let* ([step (guard-step guard)]
         [pos (guard-pos step)])
    (if (equal? pos obstacle)
        #\#
        (grid-cell grid pos))))

(define (grid-walk grid guard seen)
  (let* ([peek (guard-peek grid guard)]
         [seen (set-add seen (guard-pos guard))])
    (cond
      [(null? peek) seen]
      [(equal? #\# peek) (grid-walk grid (guard-rotate guard) seen)]
      [else (grid-walk grid (guard-step guard) seen)])))

(define (part-1 path)
  (let* ([grid (file->grid path)]
         [guard (grid-find-guard grid)])
    (set-count (grid-walk grid guard (set)))))

;; 4647
(printf "Part-1: ~a\n" (part-1 "day06-input.txt"))

(define (grid-has-loop? grid obstacle guard seen)
  (let* ([peek (guard-peek* grid obstacle guard)])
    (cond
      [(null? peek) #f]
      [(set-member? seen guard) #t]
      [(equal? #\# peek) (grid-has-loop? grid obstacle (guard-rotate guard) (set-add seen guard))]
      [else (grid-has-loop? grid obstacle (guard-step guard) (set-add seen guard))])))

(define (part-2 path)
  (let* ([grid (file->grid path)]
         [guard (grid-find-guard grid)])
    (for*/sum ([r (grid-rows grid)] [c (grid-cols grid)]
                                    #:when (equal? (grid-cell grid (list r c)) #\.))
              (if (grid-has-loop? grid (list r c) guard (set)) 1 0))))

;; 1723
(printf "Part-2: ~a\n" (part-2 "day06-input.txt"))
