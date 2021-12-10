#lang racket

(define (read-chunks path)
  (map string->list (file->lines path)))

(define open-brackets (seteqv #\( #\[ #\{ #\<))
(define closing-brackets #hasheqv((#\) . #\() (#\] . #\[) (#\} . #\{) (#\> . #\<)))
(define error-costs #hash((#\) . 3) (#\] . 57) (#\} . 1197) (#\> . 25137)))
(define completion-costs #hash((#\( . 1) (#\[ . 2) (#\{ . 3) (#\< . 4)))

(define (open? ch)
  (set-member? open-brackets ch))
(define (can-close? cur top)
  (eqv? (hash-ref closing-brackets cur #f) top))

(define (match-chunk chunk)
  (let do-match ([stack empty]
                 [chunk chunk])
    (cond
      [(empty? chunk) (cons 'success stack)]
      [(open? (car chunk)) (do-match (cons (car chunk) stack) (cdr chunk))]
      [(can-close? (car chunk) (car stack)) (do-match (cdr stack) (cdr chunk))]
      [else (cons 'failure (car chunk))])))

(define match-results
  (for/list ([chunk (read-chunks "day10-input.txt")])
    (match-chunk chunk)))

(define (part-1 match-results)
  (for/sum ([result match-results]
            #:when (eqv? 'failure (car result)))
    (hash-ref error-costs (cdr result) #f)))

(printf "Part-1: ~a\n" (part-1 match-results))

(define (vector-middle vt)
  (vector-ref (vector-sort vt <) (quotient (vector-length vt) 2)))

(define (part-2 match-results)
  (vector-middle
   (for/vector ([result match-results]
                #:when (eq? 'success (car result)))
     (for/fold ([score 0])
               ([cur (cdr result)])
       (+ (* score 5) (hash-ref completion-costs cur #f))))))

(printf "Part-2: ~a\n" (part-2 match-results))