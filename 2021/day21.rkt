#lang racket

(define (next-pos cur-pos die-roll)
  (add1 (remainder (+ (sub1 cur-pos) (+ (* die-roll 3) 3)) 10)))

(define (update-player player die-roll)
  (match-define (cons pos score) player)
  (cons (next-pos pos die-roll) (+ score (next-pos pos die-roll))))

(define (part-1 fst-pos snd-pos)
  (let play ([fst (cons fst-pos 0)]
             [snd (cons snd-pos 0)]
             [die-roll 1]
             [rolls 0])
    (cond
      [(>= (cdr fst) 1000) (* (cdr snd) (* rolls 3))]
      [(>= (cdr snd) 1000) (* (cdr fst) (* rolls 3))]
      [(= (remainder rolls 2) 0)
       (play (update-player fst die-roll) snd (+ die-roll 3) (+ rolls 1))]
      [else
       (play fst (update-player snd die-roll) (+ die-roll 3) (+ rolls 1))])))

(printf "Part-1: ~a\n" (part-1 7 1))

(define (part-2 fst-pos snd-pos)
  (define (next-states score pos)
    (for/list ([rolls (cartesian-product '(1 2 3) '(1 2 3) '(1 2 3))])
      (define rolls-sum (apply + rolls))
      (define pos* (add1 (remainder (+ (sub1 pos) rolls-sum) 10)))
      (cons (+ score pos*) pos*)))
  (define (count-win fst-score fst-pos snd-score snd-pos turn target)
    (cond
      [(and (>= fst-score 21) (eq? target 'fst)) 1]
      [(and (>= snd-score 21) (eq? target 'snd)) 1]
      [(>= fst-score 21) 0]
      [(>= snd-score 21) 0]
      [(= turn 0)
       (for/sum ([next (next-states fst-score fst-pos)])
         (count-win/memo (car next) (cdr next) snd-score snd-pos (remainder (add1 turn) 2) target))]
      [else
       (for/sum ([next (next-states snd-score snd-pos)])
         (count-win/memo fst-score fst-pos (car next) (cdr next) (remainder (add1 turn) 2) target))]))
  (define seen (make-hash))
  (define (count-win/memo fst-score fst-pos snd-score snd-pos turn target)
    (define kw `(,fst-score ,fst-pos ,snd-score ,snd-pos ,turn ,target))
    (cond
      [(hash-has-key? seen kw) (hash-ref! seen kw #f)]
      [else
       (define v (count-win fst-score fst-pos snd-score snd-pos turn target))
       (hash-set! seen kw v)
       v]))
  (max (count-win 0 fst-pos 0 snd-pos 0 'fst)
       (count-win 0 fst-pos 0 snd-pos 0 'snd)))

(printf "Part-2: ~a\n" (part-2 7 1))