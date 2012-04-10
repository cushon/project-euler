#lang racket

; Find the least number for which the proportion of bouncy numbers is
; exactly 99%.

(define (solve)
  
  (define (bouncy? n)
    (define (helper2 n r f)
      (cond [(zero? n) #f]
            [(f (remainder n 10) r)
             (helper2 (quotient n 10) (remainder n 10) f)]
            [else #t]))
    (define (helper n r)
      (cond
        [(zero? n) #f]
        [(= (remainder n 10) r) (helper (quotient n 10) (remainder n 10))]
        [(< (remainder n 10) r) (helper2 n r <=)]
        [(> (remainder n 10) r) (helper2 n r >=)]))
    (and
     (>= n 100)
     (local
       ((define q (quotient n 10))
        (define r (remainder n 10)))
       (helper q r))))
  
  (define (solve for)
    (define (helper i total bouncy)
      (if (and (> total 0) 
               (> (/ bouncy total) for))
          i
          (helper (add1 i)
                  (add1 total)
                  (if (bouncy? i) (add1 bouncy) bouncy))))
    (helper 1 0 0))
  
  (- (solve (/ 99 100)) 2))

(provide solve)
