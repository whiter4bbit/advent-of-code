#lang racket

(define (read-picked path)
  (map string->number (string-split (first (string-split (file->string path) "\n\n")) ",")))

(define (read-boards path)
  (for/list ([chunk (cdr (string-split (file->string path) "\n\n"))])
    (for/hash ([n (flatten (map string-split (string-split chunk "\n")))]
               [i (in-naturals)])
      (values (string->number n) i))))

;  0  1  2  3  4
;  5  6  7  8  9
; 10 11 12 13 14
; 15 16 17 18 19
; 20 21 22 23 24
(define streak-indices '((0 1 2 3 4)
                         (5 6 7 8 9)
                         (10 11 12 13 14)
                         (15 16 17 18 19)
                         (20 21 22 23 24)
                         (0 5 10 15 20)
                         (1 6 11 16 21)
                         (2 7 12 17 22)
                         (3 8 13 18 23)
                         (4 9 14 19 24)))

(define (streak? board)
  (for/or ([indices streak-indices])
    (for/and ([index indices])
      (not (member index (hash-values board))))))

(define (winning-sum board num)
  (* (apply + (hash-keys board)) num))

(define (update-boards boards num)
  (for/list ([board boards])
    (hash-remove board num)))

(define (play picked boards [winners null])
  (cond
    [(or (empty? picked) (empty? boards)) winners]
    [else
     (define num (car picked))
     (for/fold ([winners winners]
                [keep null]
                #:result (play (cdr picked) keep winners))
               ([board (update-boards boards num)])
       (cond
         [(streak? board) (values (append winners (list (winning-sum board num))) keep)]
         [else (values winners (append keep (list board)))]))]))

(define results (play (read-picked "day4-input.txt") (read-boards "day4-input.txt")))

(printf "Part-1: ~a\n" (first results))
(printf "Part-2: ~a\n" (last results))