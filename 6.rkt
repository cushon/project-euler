#lang racket
(require "common.rkt")

; Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

(define (solve)
  (define l (range 1 100))
  (- ((lambda (x) (* x x)) (foldl + 0 l))
     (foldl + 0 (map (lambda (x) (* x x)) l))))

(provide solve)