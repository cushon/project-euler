#lang racket
(require rackunit "common.rkt")

; Find the number of fractions with more digits in the numerator than
; denominator, in the first 1000 decimal expansions of sqrt(2).

(define (solve)
  
  (define (approx n)
    (define (h n)
      (if (zero? n) 0
          (/ 1 (+ 2 (h (sub1 n))))))
    (+ 1 (h n)))
  
  (check-equal? (approx 1) (/ 3 2))
  (check-equal? (approx 2) (/ 7 5))
  (check-equal? (approx 3) (/ 17 12))
  (check-equal? (approx 4) (/ 41 29))
  (check-equal? (approx 5) (/ 99 70))
  (check-equal? (approx 6) (/ 239 169))
  (check-equal? (approx 7) (/ 577 408))
  
  (length
   (filter
    (lambda (x) (> (length (int->list (numerator x)))
                   (length (int->list (denominator x)))))
    (map approx (range 1 1000)))))

(provide solve)
  
  