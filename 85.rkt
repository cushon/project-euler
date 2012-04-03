#lang racket

; Although there exists no rectangular grid that contains exactly two million
; rectangles, find the area of the grid with the nearest solution.

(define (solve)
  
  (define (sum-up n) (/ (* n (add1 n)) 2))
  
  (define (rects a b) (* (sum-up a) (sum-up b)))
  
  (define (find cap)
    (define (helper a b best)
      (cond
        [(> b cap) best]
        [(> a cap) (helper 1 (add1 b) best)]
        [(local ((define cand (rects a b)))
           (if (and (< cand 2000000)
                    (> cand (first best)))
               (helper (add1 a) b (list cand (list a b)))
               (helper (add1 a) b best)))]))
    (helper 1 1 (list 0)))
  
  (apply * (second (find 100))))

(provide solve)