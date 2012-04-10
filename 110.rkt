#lang racket
(require "common.rkt")

; Find the least value of n for which the number of distinct integer solutions
; to 1/x + 1/y = 1/n exceeds four million.

(define (solve)
  
  (define cap 4000000)
  
  (define (prime-list size)
    (define (h n trial)
      (cond [(zero? n) '()]
            [(is-prime? trial) (cons trial (h (sub1 n) (add1 trial)))]
            [(h n (add1 trial))]))
    (h size 1))
  
  (define ps (prime-list (inexact->exact (ceiling (/ (log cap) (log 3))))))
  
  (define (trials cap len)
    (define (h cap len acc all)
      (if (zero? len)
          (cons (reverse acc) all)
          (foldl (lambda (x y) (h x (sub1 len) (cons x acc) y))
                 all
                 (range 0 cap))))
    (h cap len '() '()))
  
  (define x (trials 3 (inexact->exact (ceiling (/ (log cap) (log 3))))))
  
  (first
   (sort
    (map (lambda (x) (foldl * 1 (map (lambda (x) (apply expt x)) x)))
         (map (lambda (x) (zip ps x))
              (filter (lambda (x) 
                        (> (foldl * 1 
                                  (map (lambda (x) (+ (* x 2) 1)) 
                                       (filter (lambda (x) (not (zero? x))) x)))
                           8000000))
                      x)))
    <)))

(provide solve)