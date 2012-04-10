#lang racket
(require "common.rkt")

; Find the sum of maximal remainders of [(a+1)^2+(a-1)^2]/a^2 for 3 <= a <= 1000.

(define (solve)
  (foldl + 0 (map (lambda (x) (if (even? x) 
                                  (* x (- x 2))
                                  (- (* x x) x)))
                  (range 3 1000))))

(provide solve)