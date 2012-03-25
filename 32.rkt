#lang racket
(require "common.rkt")

; Find the product of all 1-9 pandigital multiplier, multiplicand, product
; 3-tuples.

(define (solve)
  
  (define l (filter pandigital? (range 1 12345)))
  
  (define out (make-hash))
  
  (for-each 
   (lambda (x)
     (for-each 
      (lambda (y) 
        (if (strictly-pandigital? x y (* x y)) 
            (hash-set! out (* x y) #t) (void)))
      l)) l)
  
  (foldl + 0 (hash-keys out)))

(provide solve)