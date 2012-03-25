#lang racket
(require "common.rkt")

; Find the sum of the digits in the number 100!

(define (solve)
  (define (! n) (if (zero? n) 1 (* n (! (sub1 n)))))
  (foldl + 0(int->list (! 100))))

(provide solve)