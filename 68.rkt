#lang racket
(require "common.rkt")
         
; Find the maximum 16-digit string for a "magic" 5-gon ring.

(define (solve)

  (define axis-sum (/ (+ (* 2 (foldl + 0 (range 1 5)))
                         (foldl + 0 (range 6 10)))
                      5))
  
  (define (gen-sets lst b)
    (if (empty? lst) '()
        (local ((define c (- axis-sum (car lst) b)))
          (cons (list (car lst) b c)
                (gen-sets (cdr lst) c)))))
  
  (define (glue-digits x)
    (list->int (flatten (map int->list (flatten x)))))
  
  (define set (gen-sets '(10 9 8 7 6) (- axis-sum 10 1)))
  
  (glue-digits (cons (last set) (drop-right set 1))))

(provide solve)