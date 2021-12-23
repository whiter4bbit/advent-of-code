#lang racket

(define (read-rooms path)
  (define lines (file->lines path))
  (for/list ([i (in-range 3 10 2)])
    (for/list ([j (in-range 2 (sub1 (length lines)))])
      (define amp (string-ref (list-ref lines j) i))
      (- (char->integer amp) (char->integer #\A)))))

(define costs #(1 10 100 1000))

(define inf-energy 1000000000)

(define (cost-to-organize rooms [hall '(#f #f #f #f #f #f #f #f #f #f #f)])
  (define room-size (/ (apply + (map length rooms)) 4))
  (define (final? rooms)
    (for/and ([(room i) (in-indexed rooms)])
      (and (= (length room) room-size)
           (for/and ([amp room])
             (= amp i)))))
  (define (can-move-from-room? room room-label)
    (define unique (remove-duplicates room))
    (cond
      [(empty? unique) #f]
      [(= (length unique) 1) (not (= (car unique) room-label))]
      [else #t]))
  (define (path-clear? hall i j [v #f])
    (for/and ([k (in-range (min i j) (add1 (max i j)))])
      (or (eq? (list-ref hall k) #f) (eq? (list-ref hall k) v))))
  (define (room-label->room-loc label)
    (+ 2 (* 2 label)))
  (define (room->hall-cost room room-label hall-loc)
    (define amp (car room))
    (define room-loc (room-label->room-loc room-label))
    (define hall-distance (add1 (- (max room-loc hall-loc) (min room-loc hall-loc))))
    (* (+ hall-distance (- room-size (length room))) (vector-ref costs amp)))
  (define (room->hall rooms hall)
    (for/fold ([energy inf-energy])
              ([(room room-label) (in-indexed rooms)]
               #:when (can-move-from-room? room room-label))
      (define room-loc (room-label->room-loc room-label))
      (for/fold ([energy energy])
                ([hall-loc '(0 1 3 5 7 9 10)]
                 #:when (path-clear? hall room-loc hall-loc))
        (define rooms* (list-set rooms room-label (cdr room)))
        (define hall* (list-set hall hall-loc (car room)))
        (min (+ (room->hall-cost room room-label hall-loc) (organize/memo rooms* hall*))
             energy))))
  (define (can-go-to-room? rooms hall amp hall-loc)
    (define room (list-ref rooms amp))
    (define room-unique (remove-duplicates room))
    (and (path-clear? hall (room-label->room-loc amp) hall-loc amp)
         (or (empty? room)
             (and (= (length room-unique) 1)
                  (< (length room) room-size)
                  (= (car room-unique) amp)))))
  (define (hall->room rooms hall)
    (for/fold ([energy inf-energy])
              ([(amp hall-loc) (in-indexed hall)]
               #:when (and amp (can-go-to-room? rooms hall amp hall-loc)))
      (define room (list-ref rooms amp))
      (define room-loc (room-label->room-loc amp))
      (define moves (- (max room-loc hall-loc) (min room-loc hall-loc)))
      (define cost (* (+ moves (- room-size (length room))) (vector-ref costs amp)))
      (define rooms* (list-set rooms amp (cons amp room)))
      (define hall* (list-set hall hall-loc #f))
      (min (+ cost (organize/memo rooms* hall*))
           energy)))
  (define (organize rooms hall)
    (cond
      [(final? rooms) 0]
      [else
       (min (room->hall rooms hall)
            (hall->room rooms hall))]))
  (define seen (make-hash))
  (define (organize/memo rooms hall)
    (define kw `(,rooms ,hall))
    (cond
      [(hash-has-key? seen kw) (hash-ref! seen kw #f)]
      [else
       (define v (organize rooms hall))
       (hash-set! seen kw v)
       v]))
  (organize/memo rooms hall))

(module+ test
  (require rackunit)
  (check-equal? (cost-to-organize (read-rooms "day23-input-p1.txt")) 13520)
  (check-equal? (cost-to-organize (read-rooms "day23-test-p1.txt")) 12521)
  (check-equal? (cost-to-organize (read-rooms "day23-input-p2.txt")) 48708)
  (check-equal? (cost-to-organize (read-rooms "day23-test-p2.txt")) 44169))