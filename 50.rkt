#lang racket
(require "common.rkt")

; Find which prime below one-million can be written as the sum of the most
; consecutive primes.

(define (solve)
  
  (define (max-prefix l stop)
    (define (h l cur cur-c max max-c)
      (cond [(or (empty? l) (> cur stop)) (list max max-c)]
            [(and (is-prime? cur) (> cur-c max-c))
             (h (cdr l) (+ (car l) cur) (add1 cur-c) cur cur-c)]
            [else (h (cdr l) (+ (car l) cur) (add1 cur-c) max max-c)]))
    (h l 0 0 0 0))
  
  (define (max-prime lst stop)
    (define (h l max)
      (cond [(or (empty? l) (> (car l) stop)) max]
            [else (local ((define c (max-prefix l stop)))
                    (if (> (second c) (second max))
                        (h (cdr l) c)
                        (h (cdr l) max)))]))
    (h lst '(0 0)))
  
  (first (max-prime (filter is-prime? (range 1 1000000)) 1000000)))

(provide solve)