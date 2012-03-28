#lang racket
(require "common.rkt")

; Find the maximum digital sum of a^b for a, b < 100.

(define (solve)
  (foldl max 0
         (map (lambda (x) (foldl + 0 (int->list x)))
              (map (lambda (x) (apply expt x))
                   (cartesian-product (range 1 100) (range 1 100))))))

(provide solve)
  