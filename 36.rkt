#lang racket
(require "common.rkt")

; Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.

(define (solve)
  
  (define (int->list_2 n)
    (define (h n acc)
      (if (zero? n) acc
          (h (quotient n 2) (cons (remainder n 2) acc))))
    (h n '()))
  
  (define (pal-list? lst)
    (equal? lst (reverse lst)))
  
  (define (both-pal? x)
    (and (pal-list? (int->list x))
         (pal-list? (int->list_2 x))))
  
  (foldl + 0 (filter both-pal? (range 1 1000000))))

(provide solve)