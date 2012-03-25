#lang racket
(require "common.rkt")

; Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.

(define (solve)  
  (define (sum-pow n x) (foldl + 0 (map (lambda (x) (expt x n)) (int->list x))))
  (foldl + 0 (filter (lambda (x) (= (sum-pow 5 x) x)) (range 5 1000000))))

(provide solve)
  
  