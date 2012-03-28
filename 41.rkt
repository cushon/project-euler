#lang racket
(require "common.rkt")

; Find the largest existing n-digit pandigital prime.

(define (solve)
  
  (define (find-with-length n)
    
    (define (try-perms lst acc len perm)
      (cond [(zero? len) (and (is-prime? perm) perm)]
            [(empty? lst) #f]
            [else
             (or
              (try-perms (append (reverse acc) (cdr lst))
                         '()
                         (sub1 len)
                         (+ (* 10 perm) (car lst)))
              (try-perms (cdr lst)
                         (cons (car lst) acc) 
                         len
                         perm))]))
    
    (try-perms (reverse (range 1 n)) '() n 0))
  
  (define (try-lengths n)
    (and (> n 0) (or (find-with-length n) (try-lengths (sub1 n)))))
  
  (try-lengths 9))

(provide solve)