#lang racket

; Find wow many different ways £2 can be made using any number of coins.

(define (solve)
  
  (define denom (reverse '(1 2 5 10 20 50 100 200)))
  
  (define (w n)
    (define (s n r acc)
      (cond [(zero? n) 1]
            [(empty? r) 0]
            [else (next n r acc)]))
    (define (next n r acc)
      (+ (s n (cdr r) acc)
         (if (< n (car r))
             0
             (next (- n (car r)) r (add1 acc)))))
    (s n denom 0))
  
  (w 200))

(provide solve)