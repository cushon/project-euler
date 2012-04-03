#lang racket
(require "common.rkt")

(define (solve)
  
  (define primes (make-hash))
  
  (define (build-primes cap)
    (define (helper n)
      (if (>= n cap) '()
          (if (is-prime? n)
              (cons n (helper (+ n 2)))
              (helper (+ n 2)))))
    (cons 2 (helper 3)))
  
  (define primev (list->vector (build-primes 10000)))
  
  (define (getp n) (vector-ref primev n))
  
  (define (solve for)
    (define (helper a b c count)
      (cond
        [(> (expt (getp c) 4) for) '()]
        [(> (expt (getp a) 2) for) (helper 0 (add1 b) c count)]
        [(> (expt (getp b) 3) for) (helper a 0 (add1 c) count)]
        [else
         (local ((define num (+ (expt (getp a) 2)
                                (expt (getp b) 3)
                                (expt (getp c) 4))))
           (if (< num for)
               (cons num (helper (add1 a) b c (add1 count)))
               (helper (add1 a) b c count)))]))
    (helper 0 0 0 0))
  
  (length (remove-duplicates (solve 50000000))))

(provide solve)