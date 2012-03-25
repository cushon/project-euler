#lang racket
(require "common.rkt")

; Find the sum of the digits in 100!

(define (solve)
  (foldl + 0 (int->list (factorial 100))))

(provide solve)