#lang racket

; Find the first arrangement of red and blue balls where the probability of
; selecting 2 blue balls is 1/2 and there are more than 10^12 balls.

(define (solve)
  
  (define (recur x y) (list (+ (* 3 x) (* 2 y) -2) (+ (* 4 x) (* 3 y) -3)))
  
  (define (h n curr)
    (if (> (+ (first curr) (second curr)) (expt 10 12))
        (first curr)
        (h (add1 n) (apply recur curr))))
  
  (h 1 (list 1 1)))

(provide solve)