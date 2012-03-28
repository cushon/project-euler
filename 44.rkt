#lang racket
(require "common.rkt")

; Find the pair of pentagonal numbers, Pj and Pk, for which their sum and
; difference is pentagonal and D = |Pk  Pj| is minimised

(define (solve)
  
  (define (pn n)
    (/ (* n (sub1 (* 3 n))) 2))
  
  (define (pni n)
    (/ (+ 1 (sqrt (+ 1 (* 24 n)))) 6))
  
  (define l
    (filter (lambda (x) (< (first x) (second x)))
            (cartesian-product (map pn (build-list 2500 add1))
                               (map pn (build-list 2500 add1)))))
  
  ((lambda (x) (abs (- (first x) (second x))))
   (first
    (filter (lambda (x)
              (match x
                [(list a b) (and (integer? (pni (abs (- a b))))
                                 (integer? (pni (abs (+ a b)))))]))
            l))))

(provide solve)