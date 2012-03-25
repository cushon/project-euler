#lang racket

; Find the number under 100 with the longest collatz chain.

(define (solve)

  (define (collatz-length n)
    (define (helper n acc)
      (cond
        [(= 1 n) acc]
        [(even? n) (helper (/ n 2) (add1 acc))]
        [(helper (+ (* 3 n) 1) (add1 acc))]))
    (helper n 1))
  
  (define (find-max n)
    (define (mx a b)
      (if (> (second a) (second b)) a b))
    (define (helper i m)
      (if (< i n) (helper (add1 i) (mx (list i (collatz-length i)) m)) m))
    (helper 1 '(1 1)))
  
  (first (find-max 1000000)))

(provide solve)