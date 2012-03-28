#lang racket
(require "common.rkt")

; Find the first four consecutive integers to have four distinct primes factors.

(define (solve)
  
  (define prime-factors (memo-factory prime-factor))
  
  (define (h i seen)
    (if (= seen 4) (- i 4)
        (h (add1 i) 
           (if (= 4 (length (remove-duplicates (prime-factors i))))
               (add1 seen)
               0))))
  
  (h 1 0))

(provide solve)