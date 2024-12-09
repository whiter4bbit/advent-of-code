#lang racket

(define (board-rows board)
  (vector-length board))

(define (board-cols board)
  (vector-length (vector-ref board 0)))

(define (board-ref board r c)
  (vector-ref (vector-ref board r) c))

(define (pos-valid? board r c)
  (and (>= r 0) (>= c 0) (< r (board-rows board)) (< c (board-cols board))))

(define (find-word-at board word r c r-offset c-offset)
  (cond
    [(not (pos-valid? board r c)) 0]
    [(equal? (board-ref board r c) (car word))
     (if (null? (cdr word))
         1
         (find-word-at board (cdr word) (+ r r-offset) (+ c c-offset) r-offset c-offset))]
    [else 0]))

(define (row->vector row)
  (list->vector (string->list row)))

(define (file->board path)
  (let* ([rows (file->lines path)]) (list->vector (map row->vector rows))))

(define (offsets)
  (for*/list ([r-offset (list -1 0 1)]
              [c-offset (list -1 0 1)]
              #:when (or (not (equal? c-offset 0)) (not (equal? r-offset 0))))
    (cons r-offset c-offset)))

(define (part-1 path)
  (let ([board (file->board path)])
    (for*/sum ([r (in-range (board-rows board))] [c (in-range (board-cols board))] [offset (offsets)])
              (find-word-at board (string->list "XMAS") r c (car offset) (cdr offset)))))

(printf "Part-1: ~a\n" (part-1 "day04-input.txt"))

(define x-mas-pattern '(#\A #\M #\S))

(define (find-x-mas board mid-r mid-c)
  (let* ([left (list (board-ref board (- mid-r 1) (- mid-c 1))
                     (board-ref board mid-r mid-c)
                     (board-ref board (+ mid-r 1) (+ mid-c 1)))]
         [right (list (board-ref board (- mid-r 1) (+ mid-c 1))
                      (board-ref board mid-r mid-c)
                      (board-ref board (+ mid-r 1) (- mid-c 1)))]
         [sorted-left (sort left char<?)]
         [sorted-right (sort right char<?)])
    (and (equal? sorted-left x-mas-pattern) (equal? sorted-right x-mas-pattern))))

(define (part-2 path)
  (let ([board (file->board path)])
    (for*/sum ([r (in-range 1 (- (board-rows board) 1))] [c (in-range 1 (- (board-cols board) 1))]
                                                         #:when (equal? #\A (board-ref board r c)))
              (if (find-x-mas board r c) 1 0))))

(printf "Part-2: ~a\n" (part-2 "day04-input.txt"))
