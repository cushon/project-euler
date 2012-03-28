#lang racket
(require "common.rkt")

; Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x,
; contain the same digits.

(define (solve)

  (define (pred x)
    (= 1 (length (remove-duplicates
                  (map (lambda (y) (sort (int->list (* x y)) <))
                       (range 1 6))))))

  (define (h n) (if (pred n) n (h (add1 n))))
  
  (h 1))

(provide solve)
