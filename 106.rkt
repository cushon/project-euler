#lang racket
(require "common.rkt")

; For a set of size 12, find how many subset pairs need to be tested to confirm
; that no two subsets have equal sum, and subsets with greater sizes have
; greater sums.

(define (solve)
  
  (define (cat a b c)
    (if (and (zero? b) (zero? c)) 1
        (+ (if (> b 0) (cat (add1 a) (sub1 b) c) 0)
           (if (and (> a 0) (> c 0)) (cat (sub1 a) b (sub1 c)) 0))))
  
  (define (non-dom set-size n)
    (- (/ (* (choose set-size n) (choose (- set-size n) n)) 2)
       (* (choose set-size (* 2 n)) (cat 0 n n))))
  
  (foldl + 0 (map (lambda (x) (non-dom 12 x)) (range 2 (quotient 12 2)))))

(provide solve)