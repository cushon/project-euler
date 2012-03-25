#lang racket
(require "common.rkt")

; Find the number of circular primes <= 1000000.

(define (solve)
  
  (define (rotations l)
    (define (h l acc)
      (if (empty? l) '()
          (cons (append l (reverse acc)) (h (cdr l) (cons (car l) acc)))))
    (h l '()))
  
  (length (filter
           (lambda (x)
             (and (is-prime? x)
                  (foldl (lambda (x y) (and x y)) #t
                         (map is-prime? (map list->int (rotations (int->list x)))))))
           (range 1 1000000))))

(provide solve)