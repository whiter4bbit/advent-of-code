#lang racket

(define (rotations coord)
  (match-define (list x y z) coord)
  (for*/list ([p (permutations (list x y z))]
              [c (cartesian-product '(1 -1) '(1 -1) '(1 -1))])
    (map * p c)))

(define (rotations* sc)
  (define transposed
    (for/list ([pos sc])
      (rotations pos)))
  (for/list ([i (length (first transposed))])
    (for/list ([row transposed])
      (list-ref row i))))

(define (read-scanners path)
  (for/list ([scanner (string-split (file->string path) "\n\n")])
    (for/list ([pos (rest (string-split scanner "\n"))])
      (map string->number (string-split pos ",")))))

(define (coord- a b)
  (map - a b))

(define (find-second-offset/rotation sc-1 sc-2*)
  (for/or ([first-pos sc-1])
    (for/or ([(sc-2 i) (in-indexed sc-2*)])
      (for/or ([second-pos sc-2])
        (define offsets-1
          (for/fold ([offsets (set)])
                    ([pos sc-1])
            (set-add offsets (coord- pos first-pos))))
        (define intersection
          (for/sum ([pos sc-2]
                    #:when (set-member? offsets-1 (coord- pos second-pos)))
            1))
        (and (> intersection 11) (cons (coord- second-pos first-pos) i))))))

(define (find-beacons/offsets sc-1 scanners* [offsets '((0 0 0))])
  (cond
    [(empty? scanners*) (cons sc-1 offsets)]
    [else
     (for/or ([(sc* i) (in-indexed scanners*)])
       (define offset/rotation (find-second-offset/rotation sc-1 sc*))
       (cond
         [offset/rotation
          (match-define (cons offset rotation) offset/rotation)
          (define sc-1*
            (for/fold ([total sc-1])
                      ([pos (list-ref sc* rotation)])
              (set-add total (coord- pos offset))))
          (define scanners** (append (take scanners* i) (drop scanners* (add1 i))))
          (find-beacons/offsets sc-1* scanners** (cons offset offsets))]
         [else #f]))]))

(define (part-1 scanners)
  (define scanners* (map rotations* (rest scanners)))
  (define beacons (car (find-beacons/offsets (list->set (first scanners)) scanners*)))
  (set-count beacons))

(define (part-2 scanners)
  (define scanners* (map rotations* (rest scanners)))
  (define offsets (cdr (find-beacons/offsets (list->set (first scanners)) scanners*)))
  (for*/fold ([max-manh 0])
             ([a offsets]
              [b offsets])
    (define abs* (map (λ (a b) (abs (- a b))) a b))
    (max max-manh (apply + abs*))))

; (printf "Part-1: ~a\n" (part-1 (read-scanners "day19-input.txt")))
; (printf "Part-2: ~a\n" (part-2 (read-scanners "day19-input.txt")))

(module+ test
  (require rackunit)
  (define scanners (read-scanners "day19-test.txt"))
  (check-equal? (part-1 scanners) 79)
  (check-equal? (part-2 scanners) 3621))