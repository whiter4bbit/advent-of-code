#lang racket

(require racket/match)

(define (read-algo path)
  (list->vector (string->list (first (file->lines path)))))

(struct image (pts inv) #:transparent)

(define (image-group img r c)
  (for*/fold ([group 0])
             ([r* (in-range (- r 1) (+ r 2))]
              [c* (in-range (- c 1) (+ c 2))])
    (define bit (if (image-lit? img r* c*) 1 0))
    (bitwise-ior (arithmetic-shift group 1) bit)))

(define (image-lit? img r c)
  (if (image-inv img)
      (not (set-member? (image-pts img) (cons r c)))
      (set-member? (image-pts img) (cons r c))))

(define (image-dims img)
  (for/fold ([min-r #f] [max-r 0] [min-c #f] [max-c 0])
            ([pt (in-set (image-pts img))])
    (match-define (cons r c) pt)
    (values (min (or min-r r) r) (max max-r r) (min (or min-c c) c) (max max-c c))))

(define (image-apply img algo)
  (define-values [min-r max-r min-c max-c] (image-dims img))
  (define inf-grp (image-group img (- min-r 100000) (- min-c 100000)))
  (define inv? (eq? (vector-ref algo inf-grp) #\#))
  (for*/fold ([pts (set)]
              #:result (image pts inv?))
             ([r (in-range (- min-r 2) (+ max-r 3))]
              [c (in-range (- min-c 2) (+ max-c 3))])
    (define i (image-group img r c))
    (cond
      [(and inv? (eq? (vector-ref algo i) #\.)) (set-add pts (cons r c))]
      [(and (not inv?) (eq? (vector-ref algo i) #\#)) (set-add pts (cons r c))]
      [else pts])))

(define (image-pts-count img)
  (set-count (image-pts img)))

(define (read-image path)
  (define lines (drop (file->lines path) 2))
  (for/fold ([pts (set)]
             #:result (image pts #f))
            ([(row i) (in-indexed lines)])
    (for/fold ([pts pts])
              ([(cell j) (in-indexed row)]
               #:when(eq? cell #\#))
      (set-add pts (cons i j)))))

(define (count-lit img algo [times 2])
  (for/fold ([res img]
             #:result (image-pts-count res))
            ([i (in-range times)])
    (image-apply res algo)))

(printf "Part-1: ~a\n" (count-lit (read-image "day20-input.txt") (read-algo "day20-input.txt")))
(printf "Part-2: ~a\n" (count-lit (read-image "day20-input.txt") (read-algo "day20-input.txt") 50))

(module+ test
  (require rackunit)
  (define image (read-image "day20-test.txt"))
  (define algo (read-algo "day20-test.txt"))
  (check-equal? (image-group image 2 2) 34)
  (check-equal? (count-lit image algo) 35)
  (check-equal? (count-lit image algo 50) 3351))