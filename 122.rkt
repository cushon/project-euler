#lang racket

; Find the sum of the minimum number of multiplications required to calculate
; x^n for 1 <= n <= 200.

(define (solve)
  
  (define cap 200)
  
  (define (it n pieces depth best)
    (if (> n cap)
        best
        (foldl
         (lambda (x best)
           (if (<= (add1 depth) (hash-ref best (+ x n) +inf.0))
               (it (+ x n) (cons n pieces) (add1 depth)
                   (hash-set best (+ x n) (add1 depth)))
               best))
         best
         (cons n pieces))))
  
  (define best (it 2 '(1) 1 (make-immutable-hash '((1 . 0) (2 . 1)))))
  
  (foldl + 0 (map cdr (filter (lambda (x) (<= (car x) 200)) (hash->list best)))))

(provide solve)