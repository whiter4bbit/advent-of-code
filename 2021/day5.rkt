#lang racket

(struct vents-line (start end dir) #:transparent)
(define (vents-line-straight? vt)
  (or (and (= (real-part (vents-line-dir vt)) 0) (= (abs (imag-part (vents-line-dir vt))) 1))
      (and (= (imag-part (vents-line-dir vt)) 0) (= (abs (real-part (vents-line-dir vt))) 1))))
(define (vents-line-straight-or-diagonal? vt)
  (or (vents-line-straight? vt)
      (and (= (abs (real-part (vents-line-dir vt))) 1)
           (= (abs (imag-part (vents-line-dir vt))) 1))))
(define (vents-line-collect-points vt points)
  (define (collect points cur)
    (let ([next-points (hash-update points cur add1 0)])
      (if (= cur (vents-line-end vt))
          next-points
          (collect next-points (+ cur (vents-line-dir vt))))))
  (collect points (vents-line-start vt)))

(define (read-lines path)
  (for/list ([raw (file->lines path)])
    (let* ([line (map string->number (regexp-split #px"\\,| \\-\\> " raw))]
           [start (+ (first line) (* (second line) 0+1i))]
           [end (+ (third line) (* (fourth line) 0+1i))]
           [diff (- end start)])
      (vents-line
       start
       end
       (/ diff (max (abs (real-part diff)) (abs (imag-part diff))))))))

(define (count-points lines type)
  (for/fold ([points (hash)]
             #:result (for/sum ([(pos c) points] #:when (> c 1)) 1))
            ([line lines]
             #:when (type line))
    (vents-line-collect-points line points)))

(printf "Part-1: ~a\n" (count-points (read-lines "day5-input.txt") vents-line-straight?))
(printf "Part-2: ~a\n" (count-points (read-lines "day5-input.txt") vents-line-straight-or-diagonal?))