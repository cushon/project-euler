
#lang racket
(require "common.rkt")

; Find the the thirtieth number with at least two digits that is the sum of
; the square of its digits.

(define (solve)
  
  (define (calc-ds n)
    (if (zero? n) 0 (+ (remainder n 10) (calc-ds (quotient n 10)))))
  
  (define (purefact m n)
    (or (= m n)
        (and (zero? (remainder m n))
             (purefact (quotient m n) n))))
  
  (define (check n)
    (define ds (calc-ds n))
    (and (> n 9) (> ds 1) (purefact n ds)))
  
  (define (gen cap vcap)
    (define (h i found)
      (if (zero? (modulo i 100000)) (printf "~a ~a\n" i found) (void))
      (cond [(or (> found cap) (> i vcap)) '()]
            [(check i) (cons i (h (add1 i) (add1 found)))]
            [(h (add1 i) found)]))
    (h 10 0))
  
  (define (find many)
    (define (h x trial found last)
      (cond [(= found many) last]
            [(check trial) 
             (printf "~a ~a\n" x trial)
             (h (add1 x) (add1 trial) (add1 found) trial)]
            [else (h x (add1 trial) found last)]))
    (h 1 1 0 -1))
  
  (list-ref
   (sort
    (map second
         (filter
          (lambda (x) (= (calc-ds (second x)) (first x)))
          (map
           (lambda (x) (list (first x) (apply expt x)))
           (cartesian-product (range 2 100) (range 2 10)))))
    <)
   29))

(provide solve)