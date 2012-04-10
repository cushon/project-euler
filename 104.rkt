#lang racket
(require "common.rkt")

; Find k s.t. Fk is the first fibonacci number for which the first 9 and the
; last 9 digits are 1-9 pandigital.

(define (solve)
  
  (define (first9 n) 
    (quotient n (expt 10 (max 0 (- (inexact->exact (ceiling (/ (log n) (log 10)))) 9)))))
  
  (define (last9 n) (remainder n (expt 10 9)))
  
  (define (fib k n1 n2 sn1 sn2)
    (if (and (strictly-pandigital? sn1)
             (strictly-pandigital? (first9 n1)))
        k (fib (add1 k) 
               (if (> n1 (expt 10 17)) (quotient n2 10) n2)
               (if (> n1 (expt 10 17)) (quotient (+ n1 n2) 10) (+ n1 n2))
               sn2
               (last9 (+ sn1 sn2)))))
  
  (fib 1 1 1 1 1))

(provide solve)