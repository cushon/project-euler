#lang racket
(require "common.rkt")

; Find the number of values of nCr great than 1000000 for 1 <= n <= 100.

(define (solve)
  (length
   (filter (lambda (x) (> x 1000000))
           (map
            (lambda (x) 
              (apply choose x))
            (filter (lambda (x) (>= (first x) (second x)))
                    (cartesian-product (range 1 100) (range 1 100)))))))

(provide solve)