#lang racket

; Find the number of manhattan paths from the top left to bottom right corners of
; a 20x20 grid.

(define (solve)

  (define mem (make-hash))
  
  (define (routes a b)
    (if (> a b) (routes b a)
        (cond 
          [(and (= a 1) (= b 1)) 2]
          [(or (= a 0) (= b 0)) 1]
          [(hash-has-key? mem (list a b))
           (hash-ref mem (list a b))]
          [else 
           (local 
             ((define result (+ (routes (sub1 a) b) (routes a (sub1 b)))))
             (hash-set! mem (list a b) result)
             result)])))
  
  (define (sq n) (routes n n))
  
  (sq 20))

(provide solve)
  