#lang racket
(require "common.rkt")

; Find the sum of primes below 2000000.

(define (solve)
  (foldl + 0 (filter is-prime? (range 1 2000000))))

(provide solve)