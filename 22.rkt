#lang racket
(require "common.rkt")

; Find the sum of the products of each name in the list multiplied by its alphabetical
; value.

(define (solve)
  
  (define (alphanum n)
    (add1 (- (char->integer n) (char->integer #\A))))
  
  (define (weight word)
    (foldr + 0 (map alphanum (string->list word))))
    
  (define (score lst i)
    (if (empty? lst) lst
        (cons (* (weight (car lst)) i) (score (cdr lst) (add1 i)))))
  
  (foldl + 0 (score (sort (csv->lst (file->string "22.txt")) string<?) 1)))

(provide solve)