#lang racket

; Find the sum of the numbers on the diagonals in a 1001 by 1001 spiral.

(define (solve)
  
  (define (sum-diagonals n)
    (define (helper i sum row-pos spiral-pos spiral-counter)
      (cond [(> (* spiral-counter 2) n) sum]
            [(> spiral-pos 3) (helper i sum 0 0 (add1 spiral-counter))]
            [(= row-pos (sub1 (* 2 spiral-counter)))
             (helper (add1 i) (+ i sum) 0 (add1 spiral-pos) spiral-counter)]
            [else
             (helper (add1 i) sum (add1 row-pos) spiral-pos spiral-counter)]))
    (helper 2 1 0 0 1))
  
  (sum-diagonals 1001))

(provide solve)