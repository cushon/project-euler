#lang racket
(require "common.rkt")

; Find the smallest cube for which exactly five permutations of its digits are
; cube.

(define (solve)

(define (normalize n) (sort (int->list n) <))

(define (solve cap)
  (define seen (make-hash))
  (define (h n)
    (define cube (* n n n))
    (define seen-times (cons n (hash-ref seen (normalize cube) '())))
    (if (>= (length seen-times) cap) seen-times
        (begin
          (hash-set! seen (normalize cube) seen-times)
          (h (add1 n)))))
  (h 1))

(apply min (map (lambda (x) (* x x x)) (solve 5))))

(provide solve)
