#lang racket
(require "common.rkt")

(define (solve)
  (define (proper-divisors n)
    (define lst '())
    (define (helper n acc)
      (if (empty? n) (set! lst (cons acc lst))
          (begin
            (helper (cdr n) acc)
            (helper (cdr n) (* (car n) acc)))))
    (helper (prime-factor n) 1)
    (reverse (cdr (sort (remove-duplicates lst) >))))
  
  (foldl + 0
         (filter (lambda (x) (local ((define a (foldr + 0 (proper-divisors x)))
                                     (define b (foldr + 0 (proper-divisors a))))
                               (and (not (= a b)) (= x b))))
                 (range 1 9999))))

(provide solve)
  
  