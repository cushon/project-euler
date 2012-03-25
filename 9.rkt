#lang racket
(require "common.rkt")

; Find a pythagorean triple s.t. a + b + c = 1000

(define (solve)
  (match
      (car
       (filter (lambda (x)
                 (match x [(list a b) (equal? (+ (* a a) (* b b)) 
                                              (* (- 1000 a b) (- 1000 a b)))]))
               (cartesian-product (range 1 1000) (range 1 1000))))
    [(list a b) (* a b (- 1000 a b))]))

(provide solve)