#lang racket

(require plot/utils)

(provide (rename-out [all-permutations3 permutations])
         permutate
         combinate
         sequence
         clamp
         bin
         x
         y
         z
         inc
         dec
         fac)

(define (bin lst
             bin-size
             #:init [coll '()]
             #:overlap [overlap 0])
  (when (>= overlap bin-size)
    (error "bin can't bin with overlap >= binsize"))
  (if (< (length lst) bin-size)
      coll
      (bin (drop-right lst (- bin-size overlap))
           bin-size
           #:init (cons (take-right lst bin-size)
                        coll)
           #:overlap overlap)))

(define (x vec) (vector-ref vec 0))
(define (y vec) (vector-ref vec 1))
(define (z vec) (vector-ref vec 2))
(define (inc n) (+ n 1))
(define (dec n) (- n 1))

(define (clamp value min-val max-val)
  (max min-val (min max-val value)))

(define (sequence lst)
  (let ([counter 0])
    (lambda () (begin0
              (list-ref lst counter)
            (set! counter (modulo (+ 1 counter)
                                  (length lst)))))))

(define (random-lengths count)
  (for/list ([i count])
    (+ (random 50) 5)))

(define (random-angles count)
  (for/list ([i count])
    (* (random) 2 pi)))

(define (integrate-vectors vectors)
  (cons (vector 0 0)
        (for/list ([i (length vectors)])
          (foldl v+
                 (vector 0 0)
                 (take vectors (inc i))))))

(define (shift list [n 1])
  ;; (when (> (length list) n)
  ;;   (error "shifting by amount larger than list size :("))
  (append (take-right list n) (drop-right list n)))

(define (shift-thru list [accum '()])
  (if (>= (length accum) (length list))
      accum
      (shift-thru (shift list) (cons list accum))))

(define (single? list)
  (eq? (length list) 1))

(define (fac n)
  (for/product ([i (range 1 (+ n 1))]) i))

(define (fac* n [c 1])
  "the same could be done with (for/product (range n)) :P"
    (if (<= n 1)
      (* c 1)
      (fac* (- n 1) (* c n))))

(define (custom-radix n radices)
  "binary counter, except its not base-2 but base-x for x in radices"
  (let* ([indices (range (length radices))]
         [sums (cons 1 (for/list ([n indices])
                         (foldl * 1 (take radices (inc n)))))])
    (for/list ([i indices])
      (modulo (floor (/ n (list-ref sums i)))
              (list-ref radices i)))))

(define (incremental-radix n max)
  "binary counter, except its not base-2 but base-(x+1) where x is each individual bits' index. e.g.: (incremental-radix 23 4) -> (1 2 3)"
  (map (lambda (i) (modulo (floor (/ n
                                (fac* i)))
                      (+ i 1)))
       (range 1 max)))

;; ================================ core

(define (combinate sequences number)
  (let ([selects (custom-radix number (map length sequences))])
    (for/list ([i (length sequences)])
      (list-ref (list-ref sequences i)
                (list-ref selects i)))))

(define (permutate sequence number)
  "create single permutation by consecutive shifting. comparatively more expensive than all-permutations3 for short sequences; but anything longer than ~9 is taking too long to map out completely."
  (let ([shifts (reverse (incremental-radix number (length sequence)))])
    (let recur ([seq sequence] [n shifts])
      (let ([i (+ (length n) 1)])
        (if (empty? n)
            seq
            (recur (append (drop-right seq i)
                           (shift (take-right seq i)
                                  (first n)))
                   (rest n)))))))

(define (all-permutations seq)
  "returns all permutations in a depth-nested list, e.g. '(1 2 3) -> '(1 (2 (3) 3 (2)) 3 (1 (2) 2 (1)) 2 (3 (1) 1 (3)))"
  (let ([accum '()])
    (if (eq? (length seq) 1)
        (set! accum (append accum seq))
        (for ([l (shift-thru seq)])
          (set! accum (append accum
                              (list (car l)
                                    (all-permutations (cdr l)))))))
    accum))

(define (all-permutations2 seq)
  "returns all permutations as explicid lists, e.g. '(1 2 3) -> '((2 1 3) (2 3 1) (3 2 1) (3 1 2) (1 3 2) (1 2 3))"
  (letrec ([all '()]
           [recur (lambda (seq [line '()])
                    (if (single? seq)
                        (set! all (cons (append line seq)
                                        all))
                        (for ([s (shift-thru seq)])
                          (recur (cdr s)
                                 (append line (take s 1))))))])
    (recur seq)
    all))

(define (all-permutations3 list)
  "same as all-permutations2, but more beautifully written."
  (let ([all '()])
    (let recur ([seq list] [line '()])
      (if (single? seq)
          (set! all (cons (append line seq)
                          all))
          (for ([s (shift-thru seq)])
            (recur (cdr s)
                   (append line (take s 1))))))
    all))
