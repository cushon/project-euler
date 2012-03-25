#lang racket
(require "common.rkt")

; Find the denominator of the product of the four curious fractions with two
; digits numerators and denominators.

(define (solve)
  
  (define (curious? a b)
    (and (or (= (remainder a 10) (quotient b 10))
             (= (quotient a 10) (remainder b 10)))
         (or (and (not (zero? (quotient b 10)))
                  (= (/ a b) (/ (remainder a 10) (quotient b 10))))
             (and (not (zero? (remainder b 10)))
                  (= (/ a b) (/ (quotient a 10) (remainder b 10)))))
         (< (/ a b) 1)))
  
  (denominator
   (foldr * 1
          (map (lambda (x) (/ (first x) (second x)))
               (filter
                (lambda (x) (apply curious? x))
                (cartesian-product (range 10 99) (range 10 99)))))))

(provide solve)