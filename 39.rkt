#lang racket
(require "common.rkt")

; Find p <= 1000 s.t. that number of right triangles with perimiter p is 
; maximized.

(define (solve)
  
  (define t (make-hash))
  
  (for-each
   (lambda (x) 
     (match x
       [(list a b)
        (define c (sqrt (+ (* a a) (* b b))))
        (if (and (integer? c) (<= (+ a b c) 1000))
            (hash-set! t (+ a b c) (cons (list a b c) (hash-ref t (+ a b c) '())))
            (void))]))
   (cartesian-product (range 1 1000) (range 1 1000)))
  
  (define (maxtri l)
    (define (h l max)
      (if (empty? l) max
          (h (cdr l) 
             (if (> (length (cdr (car l))) (length (cdr max))) (car l) max))))
    (h (cdr l) (car l)))
  
  (first (maxtri (hash->list t))))

(provide solve)