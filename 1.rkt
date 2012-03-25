#lang racket

; Find the sum multiples of 3 or 5 below 1000.

(define (solve)
  (foldl + 0
         (filter (lambda (x) (or (zero? (modulo x 3)) (zero? (modulo x 5))))
                 (build-list 1000 values))))

(provide solve)
