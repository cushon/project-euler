#lang racket
(require "common.rkt")

; For the first one hundred natural numbers, find the total of the digital sums
; of the first one hundred decimal digits for all the irrational square roots.

(define (solve)
  
  (define (try-split lst n)
    (define (h lst acc n)
      (if (or (empty? lst) (zero? n)) (list (reverse acc) lst)
          (h (cdr lst) (cons (car lst) acc) (sub1 n))))
    (h lst '() n))
  
  (define (solve n cap)
    (define (find-x p c)
      (define (h x)
        (if (> (* x (+ (* 20 p) x)) c) (sub1 x)
            (h (add1 x))))
      (h 1))
    (define (h remainder digits p i)
      (if (> i cap) p
          (local
            [(define take (try-split digits 2))
             (define next-digits (second take))
             (define c (+ (* remainder 100) (list->int (first take))))
             (define x (find-x p c))
             (define next-p (+ (* p 10) x))
             (define y (* x (+ (* 20 p) x)))
             (define next-remainder (- c y))]
            (h next-remainder next-digits next-p (add1 i)))))
    (h 0 (int->list n) 0 0))
  
  (foldl + 0
         (map
          (lambda (x) (foldr + 0 (take (int->list (solve x 100)) 100)))
          (filter (lambda (x) (not (integer? (sqrt x)))) (range 1 100)))))

(provide solve)