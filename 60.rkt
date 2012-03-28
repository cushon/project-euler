#lang racket
(require "common.rkt")

; Find the lowest sum for a set of five primes for which any two primes
; concatenate to produce another prime.

(define (solve)
  
  (define memd-is-prime? (memo-factory is-prime?))
  
  (define (prime-pair? a b)
    (define (log10 n) (/ (log n) (log 10)))
    (define (join a b) (+ (* (expt 10 (add1 (floor (log10 a)))) b) a))
    (and (memd-is-prime? (join a b)) (memd-is-prime? (join b a))))
  
  
  (define (next-prime n set)
    (define cand (add1 n))
    (if (> n 10000) #f
        (if (and (memd-is-prime? cand) (foldr (lambda (x y) (and x y)) #t (map (lambda (z) (prime-pair? cand z)) set)))
            cand
            (next-prime cand set))))
  
  (define (first-set size)
    (define (call-next n acc)
      (define cand (next-prime n acc))
      (if (boolean? cand)
          (call-next (car acc) (cdr acc))
          (h (cons cand acc))))
    (define (h acc)
      (if (= (length acc) size) acc
          (call-next (car acc) acc)))
    (h '(7)))
  
  (foldl + 0 (first-set 5)))

(provide solve)