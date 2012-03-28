#lang racket
(require "common.rkt")

; Find the number of 'triangle words' in the given file.

(define (solve)
  
  (define (test n)
    (= (/ (* (integer-sqrt (* 2 n))
             (add1 (integer-sqrt (* 2 n)))) 2) n))
  
  (define (alphanum n)
    (add1 (- (char->integer n) (char->integer #\A))))
  
  (define (weight word)
    (foldr + 0 (map alphanum (string->list word))))
  
  (length (filter test (map weight (csv->lst (file->string "42.txt"))))))

(provide solve)