#lang racket
(require "common.rkt")

(define (solve)
  
  (define (trunc x)
    (define (h x)
      (if (empty? x) '()
          (cons x (h (cdr x)))))
    (append (list x) (h (cdr x)) (map reverse (h (cdr (reverse x))))))
  
  (define (v? x)
    (foldr (lambda (x y) (and x y)) #t (map is-prime? (map list->int (trunc (int->list x))))))
  
  (define (h n i)
    (cond [(zero? i) '()]
          [(v? n) (cons n (h (+ n 2) (sub1 i)))]
          [(h (+ n 2) i)]))
  
  (foldl + 0 (h 11 11)))

(provide solve)