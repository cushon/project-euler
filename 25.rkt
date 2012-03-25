#lang racket

; Find the first fibonacci number with 1000 digits.

(define (solve)
  
  (define (fib-with-digits n)
    (define (helper a b i)
      (if (>= (add1 (/ (log a) (log 10))) n) i
          (helper b (+ a b) (add1 i))))
    (helper 1 1 1))
  
  (fib-with-digits 1000))

(provide solve)