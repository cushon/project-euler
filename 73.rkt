#lang racket

; Find the number of fractions between 1/3 and 1/2 in the set of reduced proper
; fractions n/d where n < d, d <= 12000.

(define (solve)
  
  (define (h den num seen)
    (if (> num (add1 (quotient den 2))) seen
        (h den
           (add1 num)
           (if (and (= (gcd den num) 1)
                    (> (/ num den) (/ 1 3))
                    (< (/ num den) (/ 1 2)))
               (add1 seen)
               seen))))
  
  (define (g den seen)
    (if (> den 12000)
        seen
        (g (add1 den) (h den (quotient den 3) seen))))
  
  (g 1 0))

(provide solve)