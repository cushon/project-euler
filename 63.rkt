#lang racket
(require rackunit "common.rkt")

; Find the number of n-digit positive integers that are also n-th powers.

(define (solve)
  
  (define (log10 n) (/ (log n) (log 10)))
  
  (define (digits n)
    (if (zero? n) 1
        (inexact->exact (round (add1 (floor (log10 n)))))))
  
  (check-equal? (digits 1234) 4)
  (check-equal? (digits 0) 1)
  (check-equal? (digits 100) 3)
  
  (foldl 
   + 0 (flatten
        (map (lambda (x) (if (= (second x) (digits (apply expt x))) 1 0))
             (cartesian-product (range 1 9) (range 1 30))))))

(provide solve)