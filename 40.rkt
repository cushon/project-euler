#lang racket
(require "common.rkt")

; Find the product of the 1st, 10th, 100th, ... 1000000th digits of the
; concatenation of the natural numbers.

(define (solve)
  (define ds (list->vector (flatten (map int->list (range 1 200000)))))
  (foldl * 1 (map (lambda (x) (vector-ref ds (sub1 (expt 10 x)))) (range 0 6))))

(provide solve)