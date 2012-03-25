#lang racket
(require "common.rkt")

; Find the product of the coefficients, a and b, for the quadratic expression
; that produces the maximum number of primes for consecutive values of n, 
; starting with n = 0.

(define (solve)
  
  (define (make-pairs n)
    (define v (range (- n) n))
    (cartesian-product v v))
  
  (define (consecutive a b)
    (define (helper n)
      (if (not (is-prime? (+ (* n n) (* a n) b))) n (helper (add1 n))))
    (helper 0))
  
  (apply * (second
            (foldr (lambda (x y) (if (> (first x) (first y)) x y)) 
                   '(0)
                   (map (lambda (x) (list (consecutive (first x) (second x)) x))
                        (make-pairs 1000))))))



(provide solve)