#lang racket

; Find the last 10 digits of 28433*2^7830457+1.

(define (solve)
  
  (define (mp p m)
    (cond [(zero? p) 1]
          [(= 1 p) 2]
          [(even? p) (modulo (expt (mp (/ p 2) m) 2) m)]
          [else (modulo (* 2 (mp (sub1 p) m)) m)]))
  
  (modulo (add1 (* 28433 (mp 7830457 (expt 10 10))))
          (expt 10 10)))

(provide solve)