#lang racket

; Find the sum of the perimeters of all almost equilateral triangles with
; integral side lengths and area and whose perimeters do not exceed one
; billion.

; Since the height of the triangle is an integer and the base is even, the
; formula is a quadratic diophantine equation.
;
;     .
;    /|\
;   / | \  a
;  /  |h \ 
; /   |   \
; ---------
;   a + 1 
;
; h^2 = a^2 - ((a+1)/2)^2
; 0 = 3a^2 - 2a - 1 - 4h^2
;
; Find the recurrence for the DE by magic...
;
; Do the same thing for the other set of triangles.

(define (solve)
  
  (define (series cap)
    (define (helper x y n)
      (if (> n cap) '()
          (cons (list x y)
                (helper (+ (* 7 x) (* 8 y) 2)
                        (+ (* 6 x) (* 7 y) 2)
                        (add1 n)))))
    (cdr (helper 1 -1 0)))
  
  (define (series2 cap)
    (define (helper x y n)
      (if (> n cap) '()
          (cons (list x y)
                (helper (+ (* 7 x) (* 8 y) -2)
                        (+ (* 6 x) (* 7 y) -2)
                        (add1 n)))))
    (cdr (helper 1 0 0)))
  
  (foldr + 0
         (filter (lambda (x) (and (> x 3) (< x (expt 10 9))))
                 (append (map (lambda (x) (sub1 (* x 3))) (map first (series 1000)))
                         (map (lambda (x) (add1 (* x 3))) (map first (series2 1000)))))))

(provide solve)