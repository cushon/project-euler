#lang racket

; By listing the set of reduced proper fractions for d <= 1000000 in ascending
; order of size, find the numerator of the fraction immediately to the left
; of 3/7.

(define (solve)
  
  (define (lower x)
    (define frac (/ (first x) (second x)))
    (list (numerator frac) (denominator frac)))
  
  (caadr
   (sort 
    (remove-duplicates
     (map lower
          (filter (lambda (x) (not (zero? (first x))))
                  (map (lambda (x) 
                         (list (inexact->exact (floor (* (/ 3 7) x))) x))
                       (build-list 1000000 add1)))))
    (lambda (x y) (> (/ (first x) (second x)) (/ (first y) (second y)))))))

(provide solve)