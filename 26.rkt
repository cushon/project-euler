#lang racket
(require "common.rkt")

; Find d < 1000 s.t. the number of recurring digits in the decimal expansion of
; 1/d is maximized.

(define (solve)
  
  (define (cycle-length n)
    
    (define (helper n i)
      (if (= 1 (remainder (expt 10 i) n)) i
          (helper n (add1 i))))
    
    (if (or (zero? (remainder n 2))
            (zero? (remainder n 5)))
        0
        ; n is co-prime with 10, so proceed by FLT...
        (helper n 1)))
  
  (first (foldr (lambda (x y) 
                  (if (< (second x) (second y)) y x))
                (list 0 0) 
                (map (lambda (x) (list x (cycle-length x))) 
                     (range 2 999)))))

(provide solve)