#lang racket

(require data/heap
         racket/match)

(struct grid (cells size) #:transparent)

(define (grid-ref gr cell)
  (vector-ref (grid-cells gr) cell))

(define (grid-has-cell? gr r c)
  (and (>= r 0) (>= c 0) (< r (grid-size gr)) (< c (grid-size gr))))

(define (grid-neighbors gr cell)
  (define-values [r c] (quotient/remainder cell (grid-size gr)))
  (define neighbors (list `(,(sub1 r) . ,c)
                          `(,(add1 r) . ,c)
                          `(,r . ,(sub1 c))
                          `(,r . ,(add1 c))))
  (for/list ([nei neighbors]
             #:when (grid-has-cell? gr (car nei) (cdr nei)))
    (+ (* (car nei) (grid-size gr)) (cdr nei))))

(define (read-grid path)
  (define chars (string->list (file->string path)))
  (grid
   (for/vector ([ch chars]
                #:when (not (equal? ch #\newline)))
     (- (char->integer ch) (char->integer #\0)))
   (index-of chars #\newline)))

(define (dijkstra gr)
  (define min-nodes (make-heap (λ (a b) (<= (car a) (car b)))))
  (define costs (make-vector (vector-length (grid-cells gr)) +inf.0))
  (define target-cell (sub1 (* (grid-size gr) (grid-size gr))))
  (vector-set! costs 0 0)
  (heap-add! min-nodes '(0 . 0))
  (let find ([node (heap-min min-nodes)])
    (match-define (cons cost cell) node)
    (heap-remove-min! min-nodes)
    (cond
      [(= cell target-cell) cost]
      [else
       (for ([nei (grid-neighbors gr cell)])
         (define cost* (+ cost (grid-ref gr nei)))
         (when (< cost* (vector-ref costs nei))
           (vector-set! costs nei cost*)
           (heap-add! min-nodes (cons cost* nei))))
       (find (heap-min min-nodes))])))

(printf "Part-1: ~a\n" (dijkstra (read-grid "day15-input.txt")))

(define (grid-expand gr)
  (define exp-size (* (grid-size gr) 5))
  (define exp-cells (make-vector (* exp-size exp-size)))
  (for* ([(v i) (in-indexed (grid-cells gr))]
         [rf (in-range 0 exp-size (grid-size gr))]
         [cf (in-range 0 exp-size (grid-size gr))])
    (define-values [r c] (quotient/remainder i (grid-size gr)))
    (define r* (+ r rf))
    (define c* (+ c cf))
    (define i* (+ (* r* exp-size) c*))
    (vector-set! exp-cells i* (add1 (remainder (sub1 (+ v (+ rf cf))) 9))))
  (grid exp-cells exp-size))

(printf "Part-2: ~a\n" (dijkstra (grid-expand (read-grid "day15-input.txt"))))