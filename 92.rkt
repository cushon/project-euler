#lang racket
(require "common.rkt")

; Repeatedly adding the sum of squares of the digits of a number will eventually
; yield 1 or 89. Frind how many of the numbers below 10000000 go to 89.

(define (solve)
  
  (define (next n)
    (foldl + 0 (map (lambda (x) (* x x)) (int->list n))))
  
  (define t (make-hash))
  
  (hash-set! t 89 89)
  (hash-set! t 1 1)
  
  (define (goes-to n)
    (hash-ref 
     t n
     (lambda ()
       (let ((result (goes-to (next n))))
         (hash-set! t n result)
         result))))
  
  (define (h i acc)
    (if (>= i 10000000) acc
        (h (add1 i) (if (= 89 (goes-to i)) (add1 acc) acc))))
  
  (h 1 0))

(provide solve)