#lang racket
(require "common.rkt")

; Find the first value that can be written as the sum of primes in over five
; thousand different ways.

(define (solve)
  
  (define seen (make-hash))
  
  (define (ways n)
    (define (h n cap)
      (if (hash-has-key? seen (list n cap))
          (hash-ref seen (list n cap))
          (local ((define result
                    (cond [(= n 0) 1]
                          [(= n 2) 1]
                          [else
                           (foldr + 0
                                  (map
                                   (lambda (x) (h (- n x) (min (- n x) x)))
                                   (get-primes 1 cap)))])))
            (hash-set! seen (list n cap) result)
            result)))
    (h n (sub1 n)))
  
  (define mem-is-prime? (memo-factory is-prime?))
  
  (define (get-primes start cap)
    (define (h n)
      (cond [(> n cap) '()]
            [(is-prime? n) (cons n (h (add1 n)))]
            [else (h (add1 n))]))
    (h start))
  
  (define (solve for)
    (define (h n)
      (if (> (ways n) for) n
          (h (add1 n))))
    (h 1))
  
  (solve 5000))

(provide solve)