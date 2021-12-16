#lang racket

(require data/heap)

(struct grid (cells size) #:transparent)
(struct cell (row col value) #:transparent)

(define (read-grid path)
  (for/fold ([cells (hash)]
             #:result (grid cells (add1 (argmax identity (map car (hash-keys cells))))))
            ([(row i) (in-indexed (file->lines path))])
    (for/fold ([cells cells])
              ([(value j) (in-indexed row)])
      (hash-set cells (cons i j) (cell i j (- (char->integer value) (char->integer #\0)))))))

(define (grid-start-cell gr)
  (hash-ref (grid-cells gr) '(0 . 0) #f))

(define (grid-target-cell gr)
  (hash-ref (grid-cells gr) (cons (sub1 (grid-size gr)) (sub1 (grid-size gr)))))

(define (grid-neighbors gr cur)
  (filter
   identity
   (map (λ (p) (hash-ref (grid-cells gr) p #f))
        (list (cons (sub1 (cell-row cur)) (cell-col cur))
              (cons (add1 (cell-row cur)) (cell-col cur))
              (cons (cell-row cur) (sub1 (cell-col cur)))
              (cons (cell-row cur) (add1 (cell-col cur)))))))

(define (dijkstra gr)
  (struct node (cell cost))
  (define min-nodes (make-heap (λ (a b) (<= (node-cost a) (node-cost b)))))
  (define costs (make-hash))
  (hash-set! costs (grid-start-cell gr) 0)
  (heap-add! min-nodes (node (grid-start-cell gr) 0))
  (let loop ([cur (heap-min min-nodes)])
    (heap-remove-min! min-nodes)
    (cond
      [(not (equal? (node-cost cur) (hash-ref! costs (node-cell cur) #f)))
       (loop (heap-min min-nodes))]
      [(equal? (node-cell cur) (grid-target-cell gr))
       (node-cost cur)]
      [else
       (for ([nei (grid-neighbors gr (node-cell cur))])
         (define next (node nei (+ (cell-value nei) (node-cost cur))))
         (when (> (hash-ref! costs nei +inf.0) (node-cost next))
           (heap-add! min-nodes next)
           (hash-set! costs nei (node-cost next))))
       (loop (heap-min min-nodes))])))

(printf "Part-1: ~a\n" (dijkstra (read-grid "day15-input.txt")))

(define (grid-expand gr)
  (define expanded-size (* (grid-size gr) 5))
  (for*/fold ([cells (hash)]
              #:result (grid cells expanded-size))
             ([(_ cur) (grid-cells gr)]
              [row-f (in-range 0 expanded-size (grid-size gr))]
              [col-f (in-range 0 expanded-size (grid-size gr))])
    (define row* (+ (cell-row cur) row-f))
    (define col* (+ (cell-col cur) col-f))
    (define value* (add1 (remainder (sub1 (+ (cell-value cur) (+ row-f col-f))) 9)))
    (hash-set cells (cons row* col*) (cell row* col* value*))))

(printf "Part-2: ~a\n" (dijkstra (grid-expand (read-grid "day15-input.txt"))))