#lang racket

; Find the smallest value M such that there exist more than 1000000 rectangular
; prisms bounded by MxMxM where the shortest distance from opposite corners
; along the surface of the solid is an integer.

(define (solve)
  
  (define (sumsq a b) (sqrt (+ (expt a 2) (expt b 2))))
  
  (define cap 100)
  
  (define (cuboids cap)
    (define (helper a b total)
      (define c (sumsq a b))
      (define new
        (if (>= a b)
            (ceiling (/ (add1 (- (* 2 b) a)) 2))
            (floor (/ a 2))))
      (cond
        [(> b cap) total]
        [(> a (* 2 b)) (helper 1 (add1 b) total)]
        [else (helper (add1 a) b (if (exact? c) (+ new total) total))]))
    (helper 1 1 0))
  
  (define (solve for)
    (define (bound n)
      (if (> (cuboids n) for) (list (/ n 2) n)
          (bound (* n 2))))
    (define (bisect low high)
      (if (= low high) low
          (local {(define n-index (quotient (+ high low) 2))
                  (define curr (cuboids n-index))}
            (if (> curr for)
                (bisect low (sub1 n-index))
                (bisect (add1 n-index) high)))))
    (apply bisect (bound 1)))
  
  (add1 (solve 1000000)))

(provide solve)