#lang racket

(define (read-heightmap path)
  (for/fold ([heightmap (hash)])
            ([(row i) (in-indexed (file->lines path))])
    (for/fold ([heightmap heightmap])
              ([(cell j) (in-indexed row)])
      (hash-set heightmap (cons i j) (- (char->integer cell) (char->integer #\0))))))

(define (neighbors heightmap pos)
  (for/fold ([neighbors empty])
            ([offset '((-1 . 0) (1 . 0) (0 . 1) (0 . -1))])
    (let ([next (cons (+ (car pos) (car offset)) (+ (cdr pos) (cdr offset)))])
      (if (hash-has-key? heightmap next)
          (cons next neighbors)
          neighbors))))

(define (cell-ref heightmap pos)
  (hash-ref heightmap pos #f))

(define (in-positions heightmap)
  (in-hash-keys heightmap))

(define (low-position? heightmap pos)
  (for/and ([nei-pos (neighbors heightmap pos)])
    (< (cell-ref heightmap pos) (cell-ref heightmap nei-pos))))

(define (part-1 heightmap)
  (for/sum ([pos (in-positions heightmap)]
            #:when (low-position? heightmap pos))
    (add1 (cell-ref heightmap pos))))

(printf "Part-1: ~a\n" (part-1 (read-heightmap "day9-input.txt")))

(define (part-2 heightmap)
  (define seen (mutable-set))
  (define (basin-size pos)
    (set-add! seen pos)
    (for/fold ([size 1])
              ([nei (neighbors heightmap pos)]
               #:when (and (not (set-member? seen nei))
                           (< (cell-ref heightmap nei) 9)
                           (< (cell-ref heightmap pos) (cell-ref heightmap nei))))
      (+ size (basin-size nei))))
  (for/fold ([sizes empty]
             #:result (apply * (take (sort sizes >) 3)))
            ([pos (in-positions heightmap)]
             #:when (low-position? heightmap pos))
    (cons (basin-size pos) sizes)))

(printf "Part-2: ~a\n" (part-2 (read-heightmap "day9-input.txt")))