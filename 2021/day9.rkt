#lang racket

(define (read-heightmap path)
  (for/fold ([heightmap (hash)])
            ([row (file->lines path)]
             [i (in-naturals)])
    (for/fold ([heightmap heightmap])
              ([cell row]
               [j (in-naturals)])
      (hash-set heightmap (cons i j) (- (char->integer cell) (char->integer #\0))))))

(define (neighbors pos)
  (for/list ([offset '((-1 . 0) (1 . 0) (0 . 1) (0 . -1))])
    (cons (+ (car pos) (car offset)) (+ (cdr pos) (cdr offset)))))

(define (low-positions heightmap)
  (define (low-pos? pos)
    (for/and ([nei (neighbors pos)])
      (< (hash-ref heightmap pos #f) (hash-ref heightmap nei +inf.0))))
  (for/list ([pos (in-hash-keys heightmap)]
             #:when (low-pos? pos))
    pos))

(define (part-1 heightmap)
  (for/sum ([pos (low-positions heightmap)])
    (add1 (hash-ref heightmap pos #f))))

(printf "Part-1: ~a\n" (part-1 (read-heightmap "day9-input.txt")))

(define (part-2 heightmap)
  (define seen (mutable-set))
  (define (basin-size pos)
    (set-add! seen pos)
    (for/fold ([size 1])
              ([nei (neighbors pos)]
               #:when (and (not (set-member? seen nei))
                           (< (hash-ref heightmap nei +inf.0) 9)
                           (< (hash-ref heightmap pos #f) (hash-ref heightmap nei -inf.0))))
      (+ size (basin-size nei))))
  (for/fold ([sizes empty]
             #:result (apply * (take (sort sizes >) 3)))
            ([pos (low-positions heightmap)])
    (cons (basin-size pos) sizes)))

(printf "Part-2: ~a\n" (part-2 (read-heightmap "day9-input.txt")))