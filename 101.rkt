#lang racket
(require "common.rkt")

; Investigate the optimum polynomial function to model the first k terms in a
; sequence.

(define (solve)
  
  (define (eval coefs x)
    (foldl 
     + 0
     (map (lambda (x) (apply * x))
          (zip
           coefs
           (map (lambda (n) (expt x n))
                (build-list (length coefs) values))))))
  
  (define (eval-bop coefs)
    (eval coefs (add1 (length coefs))))
  
  (define (n-approx n master)
    (define (helper i)
      (if (> i n) '()
          (cons
           (append
            (map (lambda (n) (expt i n)) (build-list n values))
            (list (eval master i)))
           (helper (add1 i)))))
    (eval-bop (rref (helper 1))))
  
  (define (solve-bop for)
    (map (lambda (x) (n-approx x for))
         (range 1 (sub1 (length for)))))
  
  (foldl + 0 (solve-bop '(1 -1 1 -1 1 -1 1 -1 1 -1 1))))

(provide solve)