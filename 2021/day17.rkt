#lang racket

(define (can-reach? vx vy x1 x2 y1 y2 [x 0] [y 0])
  (cond
    [(and (>= x x1) (<= x x2) (>= y y1) (<= y y2)) #t]
    [(or (> x x2) (< y y1)) #f]
    [else (can-reach? (max (sub1 vx) 0) (sub1 vy) x1 x2 y1 y2 (+ x vx) (+ y vy))]))

(define (arithm-sum n)
  (quotient (* n (add1 n)) 2))

(define (max-y x1 x2 y1 y2)
  (for*/fold ([mx 0])
             ([vx (in-range 1 (add1 x2))]
              [vy (in-range 1 (add1 (abs y1)))]
              #:when (can-reach? vx vy x1 x2 y1 y2))
    (max mx (arithm-sum vy))))

(define input '(169 206 -108 -68))

(printf "Part-1: ~a\n" (apply max-y input))

(define (count-velocities x1 x2 y1 y2)
  (for*/sum([vx (in-range 1 (add1 x2))]
            [vy (in-range y1 (add1 (abs y1)))]
            #:when (can-reach? vx vy x1 x2 y1 y2))
    1))

(printf "Part-2: ~a\n" (apply count-velocities input))

(module+ test
  (require rackunit)
  (check-equal? (max-y 20 30 -10 -5) 45)
  (check-equal? (count-velocities 20 30 -10 -5) 112))