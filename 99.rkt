#lang racket
(require "common.rkt")

; Find offset of the base exponent pair with the greatest numeric value.

(define (solve)
  
  (define input
    (map (lambda (x) (map string->number (csv->lst x))) (file->lines "99.txt")))
  
  (define (number lst)
    (map (lambda (x) (append (second x) (list (first x))))
         (zip (build-list (length lst) add1) lst)))
  
  (define (find-max lst)
    (define numb (number lst))
    (define (compare-exp ba pa bb pb) (< (* pa (log ba)) (* pb (log bb))))
    (foldl (lambda (x y) (if (compare-exp (first x) (second x)
                                          (first y) (second y)) y x))
           (car numb) (cdr numb)))
  
  (last (find-max input)))

(provide solve)