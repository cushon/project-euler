#lang racket
(require "common.rkt")

; Find the largest palindrome that is the product of two 3 digit numbers.

(define (solve)
  (apply max
         (filter is-palindrome?
                 (map (lambda (x) (apply * x))
                      (cartesian-product (range 100 999) (range 100 999))))))

(provide solve)
