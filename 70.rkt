#lang racket
(require "common.rkt")

; Find the value of n, 1 <= n <= 10^7 for which phi(n) is a permutation of n
; and the ratio n/phi(n) produces a minimum.

(define (solve)
  
  (define phi (fast-phi (uniq-factorizer 10000000)))
  
  (define (permutation? a b)
    (equal? (sort (int->list a) <)
            (sort (int->list b) <)))
  
  (first 
   (first
    (sort
     (filter
      (lambda (x) (permutation? (first x) (second x)))
      (map
       (lambda (x) (list x (phi x)))
       (range 8000000 (sub1 10000000))))
     (lambda (x y) (< (/ (first x) (second x))
                      (/ (first y) (second y))))))))
   
(provide solve)