#lang racket
(require "common.rkt")

; Find the sum of digits in the numerator of the 100th convergent of the
; continued fraction for e.

(define (solve)
  
  (define (get-list n)
    (cons 2
          (map (lambda (x)
                 (if (= 2 (remainder x 3)) (* 2 (/ (add1 x) 3)) 1))
               (build-list n add1))))
  
  (define (convergent lst)
    (define (helper lst)
      (if (empty? lst) 0
          (/ 1 (+ (car lst) (helper (cdr lst))))))
    (+ (car lst) (helper (cdr lst))))
  
  
  (foldl + 0 (int->list (numerator (convergent (get-list 99))))))

(provide solve)