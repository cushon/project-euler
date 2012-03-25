#lang racket 
(require "common.rkt")

; Find the greatest product of five consecutive digits in the 1000-digit number.

(define (solve)
  (define (helper lst len acc)
    (if (>= len 5)
        (helper (cdr lst) (sub1 len) (max (foldl * 1 (take lst 5)) acc))
        acc))
  (define n (string->number 
             (list->string 
              (filter char-numeric? 
                      (string->list (file->string "8.txt"))))))
  (define digits (int->list n))
  (helper digits (length digits) 0))

(provide solve)

