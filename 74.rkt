#lang racket
(require "common.rkt")

; Find the number of chains with a starting number less than 1000000 of length
; 60.

(define (solve)

  (define (next n) (foldr + 0 (map factorial (int->list n))))
  
  (define t (make-hash))
  
  (define (looplen n)
    (if (hash-has-key? t n)
        (hash-ref t n)
        (begin
          (hash-set! t n 0)
          (local ((define l (add1 (looplen (next n)))))
            (hash-set! t n l)
            l))))
  
  (define (hs n count)
    (if (> n 1000000) count
        (hs (add1 n)
            (if (= (looplen n) 60) (add1 count) count))))
  
  (hs 1 0))

(provide solve)