#lang racket
(require rackunit "common.rkt")

; Find the number of elements contained in the set of reduced proper fractions
; for d <= 1000000.

(define (solve)
  (define phi (fast-phi (uniq-factorizer 1000000)))
  (foldr + 0 (map phi (build-list 1000000 add1))))

(provide solve)
