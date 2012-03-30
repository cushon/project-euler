#lang racket
(require rackunit)

; Find the number of continued fractions sqrt(N) with an odd period
; for N <= 1000.

(define (solve)
  
  (define (get-period n)
    (define seen (make-hash))
    (define (start n)
      (gen2 (floor (sqrt n)) 1.0 (exact->inexact n) (floor (sqrt n))))
    (define (gen2 r a b c)
      (if (hash-has-key? seen (list r a b c))
          '()
          (local 
            ((define ap b)
             (define bp c)
             (define cp (/ (- b (* c c)) a))
             (define rp (floor (/ (+ (sqrt ap) bp) cp)))
             (define bpp (abs (- bp (* cp rp)))))
            (hash-set! seen (list r a b c) #t)
            (cons r (gen2 rp cp ap bpp)))))
    (start n))
  
  (check-equal? (get-period 2) (map exact->inexact '(1 2)))
  (check-equal? (get-period 3) (map exact->inexact '(1 1 2)))
  (check-equal? (get-period 5) (map exact->inexact '(2 4)))
  (check-equal? (get-period 6) (map exact->inexact '(2 2 4)))
  (check-equal? (get-period 7) (map exact->inexact '(2 1 1 1 4)))
  (check-equal? (get-period 23) (map exact->inexact '(4 1 3 1 8)))
  
  (length
   (filter odd?
           (map (lambda (x) (sub1 (length x)))
                (map get-period (build-list 9999 (lambda (x) (+ x 2))))))))

(provide solve)