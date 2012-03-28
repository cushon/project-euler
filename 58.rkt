#lang racket
(require "common.rkt")

; Find the side length of the square spiral for which the ratio of primes
; along both diagonals falls below 10%.

(define (solve)
  
  (define (spiral-diagonals target)
    (define (h base n prime total)
      (if (and (> total 1) (< (/ prime total) target)) (sub1 (* n 2))
          (h 
           (+ base (* (* n 2) 4))
           (add1 n)
           (+ prime
              (length (filter is-prime?
                              (map (lambda (x) (+ base (* x (* n 2))))
                                   (range 1 4)))))
           (+ total 4))))
    (h 1 1 0 1))
  
  (spiral-diagonals (/ 10 100)))

(provide solve)