#lang racket

; Find the 10001st prime.

(define (solve)
  (define (find-prime n)
    (define (is-prime? p i)
      (foldl (lambda (x y) (and  x y)) #t
             (map (lambda (x) (not (zero? (remainder i x)))) p)))
    (define (helper i p c)
      (cond [(= c n) (car p)]
            [(is-prime? p i) (helper (+ i 2) (cons i p) (add1 c))]
            [else (helper (+ i 2) p c)]))
    (helper 3 '(2) 1))
  (find-prime 10001))

(provide solve)