#lang racket

; Consider x^2 - Dy^2 = 1.

; Find the value of D  1000 in minimal solutions of x for which the largest
; value of x is obtained.

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
    (map inexact->exact (start n)))
  
  (define (an lst)
    (define v (list->vector (cdr lst)))
    (lambda (n) 
      (if (zero? n) 
          (car lst) 
          (vector-ref v (modulo (sub1 n) (vector-length v))))))
  
  (define (convergents of)
    (define ans (an '(1 1)))
    (define bns (an (get-period of)))
    (define (h a0 a1 b0 b1 n)
      (if (= (- (* a0 a0) (* of b0 b0)) 1) (list a0 b0)
          (h a1
             (+ (* (bns n) a1) (* (ans n) a0))
             b1
             (+ (* (bns n) b1) (* (ans n) b0))
             (add1 n))))
    (h (bns 0) (+ (* (bns 1) (bns 0)) (ans 1)) 1 (bns 1) 2))
  
  (define (convergent start period n)
    (define (helper pd decay)
      (cond [(zero? decay) 0]
            [(empty? pd) (helper period decay)]
            [else (/ 1 (+ (car pd) (helper (cdr pd) (sub1 decay))))]))
    (helper (cons start period) n))
  
  (first (first
          (sort
           (map (lambda (x) (cons x (convergents x)))
                (filter (lambda (x) (not (exact? (sqrt x))))
                        (build-list 999 (lambda (x) (+ x 2)))))
           (lambda (x y) (> (second x) (second y)))))))

(provide solve)