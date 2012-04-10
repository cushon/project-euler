#lang racket
(require "common.rkt")

; Find the number of ways to checkout a game of darts with a score less than
; 100.

(define (solve)
  
  (define doubles (sort (cons 50 (map (lambda (x) (* x 2)) (range 1 20))) >))
  
  (define scores 
    (sort
     (append
      '(25 50)
      (map (lambda (x) (* x 3)) (range 1 20))
      (map (lambda (x) (* x 2)) (range 1 20))
      (range 1 20))
     >))
  
  (define seen (make-hash))
  
  (define (w n)
    (define (s n r acc trace depth)
      (cond [(> depth 2) 0]
            [(zero? n) 1]
            [(empty? r) 0]
            [else (next n r acc trace depth)]))
    (define (next n r acc trace depth)
      (+ (s n (cdr r) acc trace depth)
         (if (or (< n (car r)) (> n (* (car r) (- 2 depth))))
             0
             (next (- n (car r)) r (add1 acc) (cons (car r) trace) (add1 depth)))))
    (hash-ref seen n
              (lambda ()
                (let ((r (s n scores 0 '() 0)))
                  (hash-set! seen n r)
                  r))))
  
  (define (ways n)
    (foldl
     + 0
     (map w (filter (lambda (x) (>= x 0)) 
                    (map (lambda (x) (- n x)) doubles)))))
  
  (foldl + 0 (map ways (range 2 99))))

(provide solve)