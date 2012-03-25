#lang racket
(require "common.rkt")

; Find the largest 1-9 pandigital number that can be formed as the concatenated
; product of an integer with (1,2, ... , n) where n > 1?

(define (solve)
  
  (define (h n)
    (if (strictly-pandigital? n (* 2 n))
        (list->int (append (int->list n) (int->list (* n 2))))
        (h (sub1 n))))
  
  (h 10000))

(provide solve)