#lang racket

; Find the first number after 40755 that is triangular, pentagonal, and hexagonal.

(define (solve)
  
  (define (t n) (/ (+ (* n n) n) 2))
  (define (ti n) (/ (+ -1 (sqrt (+ 1 (* 8 n)))) 2))
  
  (define (p n) (/ (* n (sub1 (* 3 n))) 2))
  (define (pi n) (/ (+ 1 (sqrt (+ 1 (* 24 n)))) 6))
  
  (define (h n) (* n (sub1 (* 2 n))))
  (define (hi n) (/ (+ 1 (sqrt (+ 1 (* 8 n)))) 4))
  
  (define (test n)
    (and (integer? (ti n))
         (integer? (pi n))
         (integer? (hi n))))
  
  (define (find n)
    (if (test (h n)) (h n)
        (find (add1 n))))
  
  (find 144))

(provide solve)