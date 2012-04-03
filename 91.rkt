#lang racket

; Find the number of right triangles with integer coordinates in the rectangle
; bounded by (0, 0) (0, 50) (50, 0) (50, 50).

(define (solve)
  
  (define (calc n)
    (+ (* 3 (* n n))
       (* 2 (inexact->exact (floor (/ (log n) (log 2)))))))
  
  (define (dist x1 x2 y1 y2)
    (+ (expt (abs (- x1 x2)) 2)
       (expt (abs (- y1 y2)) 2)))
  
  (define (solve cap)
    (define (h x1 x2 y1 y2 count)
      (cond
        [(> y2 cap) count]
        [(> x1 cap) (h 0 (add1 x2) y1 y2 count)]
        [(> x2 cap) (h x1 0 (add1 y1) y2 count)]
        [(> y1 cap) (h x1 x2 0 (add1 y2) count)]
        [(and (= x1 x2) (= y1 y2)) (h (add1 x1) x2 y1 y2 count)]
        [(or (and (= x1 0) (= y1 0))
             (and (= x2 0) (= y2 0)))
         (h (add1 x1) x2 y1 y2 count)]
        [else
         (local ((define dists
                   (sort (list (dist x1 x2 y1 y2)
                               (dist 0 x2 0 y2)
                               (dist x1 0 y1 0))
                         <)))
           (if (= (+ (first dists) (second dists)) (third dists))
               (h (add1 x1) x2 y1 y2 (add1 count))
               (h (add1 x1) x2 y1 y2 count)))]))
    (/ (h 0 0 0 0 0) 2))
  
  (solve 50))

(provide solve)