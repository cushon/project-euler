#lang racket
(require "common.rkt")

; Find the millionth lexicographic permutation of the digits 0 - 9?

(define (solve)
  
  (define (fact n)
    (if (= 1 n) n
        (* n (fact (sub1 n)))))
  
  (define (figure digits n)
    (if (zero? digits) '(0)
        (cons (quotient n (fact digits))
              (figure (sub1 digits) 
                      (- n (* (quotient n (fact digits)) (fact digits)))))))
  
  (define (take-n n lst)
    (define (helper n lst acc)
      (if (zero? n) (list (car lst) (append (reverse acc) (cdr lst)))
          (helper (sub1 n) (cdr lst) (cons (car lst) acc))))
    (helper n lst '()))
  
  (define (pretty lst digits)
    (if (empty? lst) '()
        (local ((define tmp (take-n (car lst) digits)))
          (cons (first tmp) (pretty (cdr lst) (second tmp))))))
  
  
  (list->int (pretty (figure 9 999999) (range 0 9))))

(provide solve)