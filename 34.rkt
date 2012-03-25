#lang racket
(require "common.rkt")

; Find the sum of all numbers which are equal to the sum of the factorial of their digits.

(define (solve)
  (define (! n) (if (<= n 1) 1 (* n (! (sub1 n)))))
  (define (curious? x) (= (foldr + 0 (map ! (int->list x))) x))
  (foldl + 0 (filter curious? (range 3 100000))))

(provide solve)