#lang racket
(require "common.rkt")

; Find the value of n <= 1000000 s.t. n/phi(n) is maximized.maximum.

(define (solve)
  
  (define (fast-phi n)
    (* n (foldl (lambda (x y) (* (- 1 (/ 1 x)) y)) 1
                (remove-duplicates (prime-factor n)))))
  
  (first
   (first
    (sort
     (map (lambda (x)
            (list x (/ x (fast-phi x))))
          (build-list 999999 (lambda (x) (+ x 2))))
     (lambda (x y) (> (second x) (second y)))))))

(provide solve)