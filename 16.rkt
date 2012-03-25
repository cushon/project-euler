#lang racket
(require "common.rkt")

; Find the sum of the digits of 2^1000.

(define (solve)
  (foldl + 0 (int->list (expt 2 1000))))

(provide solve)

