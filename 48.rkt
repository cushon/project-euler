#lang racket
(require "common.rkt")

; Find the last 10 digits of the series 1^1 + 2^2 + ... + 1000^1000.

(define (solve)
  
  (define (last10 n) (remainder n (expt 10 10)))
  
  (last10
   (foldr + 0
          (map (lambda (x) (last10 (expt x x)))
               (range 1 1000)))))

(provide solve)