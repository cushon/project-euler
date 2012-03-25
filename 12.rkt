#lang racket
(require "common.rkt")

; Find the first triangle number to have over five hundred divisors.

(define (solve)

  (define (condense f)
    (define (helper f last seen)
      (if (empty? f) (list (list last seen))
          (if (= (car f) last) (helper (cdr f) last (add1 seen))
              (cons (list last seen) (helper (cdr f) (car f) 1)))))  
    (helper f (car f) 0))
  
  (define (find-it n)
    (define (helper i j)
      (if (> (foldr * 1 (map add1 (map second (condense (prime-factor j))))) n) j
          (helper (add1 i) (+ i j))))
    (helper 1 0))
  
  (find-it 500))

(provide solve)