#lang racket

(struct num-node (v left right par) #:mutable #:transparent)

(define (string->num s)
  (define cur (string->list s))
  (define (read-pair)
    (define left (read))
    (set! cur (cdr cur))
    (define right (read))
    (set! cur (cdr cur))
    (define node (num-node #f left right #f))
    (set-num-node-par! left node)
    (set-num-node-par! right node)
    node)
  (define (read)
    (cond
      [(eq? (car cur) #\[)
       (set! cur (cdr cur))
       (read-pair)]
      [else
       (define v (string->number (format "~a" (car cur))))
       (set! cur (cdr cur))
       (when (string->number (format "~a" (car cur)))
         (set! v (+ (* 10 v) (string->number (format "~a" (car cur)))))
         (set! cur (cdr cur)))
       (num-node v #f #f #f)]))
  (read))

(define (num->string num)
  (define (stringify num)
    (cond
      [(num-node-v num) (list (format "~a" (num-node-v num)))]
      [else
       (define left (stringify (num-node-left num)))
       (define right (stringify (num-node-right num)))
       (append '("[") left '(",") right '("]"))]))
  (string-join (stringify num) ""))

(define (find-pair-at-level num level)
  (cond
    [(and (not (num-node-v num)) (= level 0)) num]
    [(not (num-node-v num))
     (or
      (find-pair-at-level (num-node-left num) (sub1 level))
      (find-pair-at-level (num-node-right num) (sub1 level)))]
    [else #f]))

(define (find-buddies num middle)
  (define (find-val-nodes num)
    (cond
      [(num-node-v num) (list num)]
      [(eq? num middle) (list num)]
      [else
       (append
        (find-val-nodes (num-node-left num))
        (find-val-nodes (num-node-right num)))]))
  (define val-nodes (find-val-nodes num))
  (define dbg (map (lambda (n) (num-node-v n)) val-nodes))
  (define mid-index (index-of val-nodes middle))
  (define left-buddy
    (if (> mid-index 0)
        (list-ref val-nodes (sub1 mid-index))
        #f))
  (define right-buddy
    (if (< (add1 mid-index) (length val-nodes))
        (list-ref val-nodes (add1 mid-index))
        #f))
  (values left-buddy right-buddy))

(define (explode! num)
  (define pair (find-pair-at-level num 4))
  (cond
    [(not pair) #f]
    [else
     (define-values [left-buddy right-buddy] (find-buddies num pair))
     (define par (num-node-par pair))
     (define zero (num-node 0 #f #f par))
     (if (eq? (num-node-left par) pair)
         (set-num-node-left! par zero)
         (set-num-node-right! par zero))
     (when left-buddy
       (set-num-node-v!
        left-buddy
        (+ (num-node-v left-buddy)
           (num-node-v (num-node-left pair)))))
     (when right-buddy
       (set-num-node-v!
        right-buddy
        (+ (num-node-v right-buddy)
           (num-node-v (num-node-right pair)))))
     #t]))

(define (split! num)
  (define (find-node num)
    (cond
      [(and (num-node-v num) (> (num-node-v num) 9)) num]
      [(num-node-v num) #f]
      [else
       (or
        (find-node (num-node-left num))
        (find-node (num-node-right num)))]))
  (define src (num->string num))
  (define node (find-node num))
  (cond
    [node
     (define par (num-node-par node))
     (define split-node (num-node #f #f #f par))
     (set-num-node-left!
      split-node
      (num-node
       (quotient (num-node-v node) 2)
       #f #f split-node))
     (set-num-node-right!
      split-node
      (num-node
       (quotient (add1 (num-node-v node)) 2)
       #f #f split-node))
     (define left? (eq? node (num-node-left par)))
     (define right? (eq? node (num-node-right par)))
     (if (eq? (num-node-left par) node)
         (set-num-node-left! par split-node)
         (set-num-node-right! par split-node))
     #t]
    [else #f]))

(define (num-add a b)
  (define sum (num-node #f a b #f))
  (set-num-node-par! a sum)
  (set-num-node-par! b sum)
  (let add ([sum sum])
    (cond
      [(explode! sum) (add sum)]
      [(split! sum) (add sum)]
      [else sum])))

(define (nums-sum str-nums)
  (for/fold ([sum (string->num (car str-nums))])
            ([num (cdr str-nums)])
    (num-add sum (string->num num))))

(define (magnitude num)
  (cond
    [(num-node-v num) (num-node-v num)]
    [else
     (+ (* 3 (magnitude (num-node-left num)))
        (* 2 (magnitude (num-node-right num))))]))

(define (part-1 str-nums)
  (magnitude (nums-sum str-nums)))

(printf "Part-1: ~a\n" (part-1 (file->lines "day18-input.txt")))

(define (part-2 str-nums)
  (for*/fold ([max-magnitude 0])
             ([a str-nums]
              [b str-nums] #:when (not (equal? a b)))
    (max
     max-magnitude
     (magnitude
      (num-add (string->num a) (string->num b))))))

(printf "Part-2: ~a\n" (part-2 (file->lines "day18-input.txt")))

(module+ test
  (require rackunit)
  (define nums
    '("[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]"
      "[[[5,[2,8]],4],[5,[[9,9],0]]]"
      "[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]"
      "[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]"
      "[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]"
      "[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]"
      "[[[[5,4],[7,7]],8],[[8,3],8]]"
      "[[9,3],[[9,9],[6,[4,9]]]]"
      "[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]"
      "[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"))
  (check-equal? (num->string (nums-sum nums)) "[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]")
  (check-equal? (part-1 nums) 4140))