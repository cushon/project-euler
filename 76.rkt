#lang racket
(require "common.rkt")

; Find the number of different ways one hundred can be written as a sum of at
; least two positive integers

(define (solve)
  
  (define seen (make-hash))
  
  (define (ways n)
    (define (h n cap)
      (if (hash-has-key? seen (list n cap))
          (hash-ref seen (list n cap))
          (local ((define result
                    (cond [(= n 0) 1]
                          [(= n 1) 1]
                          [else
                           (foldr + 0
                                  (map
                                   (lambda (x) (h (- n x) (min (- n x) x)))
                                   (range 1 cap)))])))
            (hash-set! seen (list n cap) result)
            result)))
    (h n (sub1 n)))
  
  (ways 100))

(provide solve)