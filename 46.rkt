#lang racket
(require "common.rkt")

; the smallest odd composite that cannot be written as the sum of a prime and twice a square

(define (solve)
  
  (define (test n)
    (not (empty?
          (filter
           (lambda (x) (integer? (sqrt (/ (- n x) 2))))
           (filter is-prime? (build-list n add1))))))
  
  (define (h n)
    (if (not (or (is-prime? n) (test n)))
        n
        (h (+ 2 n))))
  
  (h 33))

(provide solve)