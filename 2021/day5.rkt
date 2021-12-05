#lang racket

(struct vents-line (start end dir) #:transparent)
(define (vents-line-straight? vt)
  (let ([deg (abs (radians->degrees (angle (vents-line-dir vt))))])
    (or (= deg 0) (= deg 90) (= deg 180))))
(define (vents-line-collect-points vt points)
  (define (collect cur)
    (hash-update! points cur add1 0)
    (when (not (= cur (vents-line-end vt)))
      (collect (+ cur (vents-line-dir vt)))))
  (collect (vents-line-start vt)))

(define (read-lines path)
  (for/list ([raw (file->lines path)])
    (let* ([line (map string->number (regexp-split #px"\\,| \\-\\> " raw))]
           [start (make-rectangular (first line) (second line))]
           [end (make-rectangular (third line) (fourth line))]
           [diff (- end start)]
           [dir (/ diff (max (abs (real-part diff)) (abs (imag-part diff))))])
      (vents-line start end dir))))

(define (count-points lines type)
  (define points (make-hash))
  (for ([line lines] #:when(type line))
    (vents-line-collect-points line points))
  (for/sum ([(pos c) points] #:when (> c 1)) 1))

(printf "Part-1: ~a\n" (count-points (read-lines "day5-input.txt") vents-line-straight?))
(printf "Part-2: ~a\n" (count-points (read-lines "day5-input.txt") (lambda (x) #t)))