#lang racket
(require rackunit "common.rkt")

; Find the number of Lychrel numbers below ten-thousand.

(define (solve)
  
  (define (reverse-int n)
    (list->int (reverse (int->list n))))
  
  (check-equal? (reverse-int 0) 0)
  (check-equal? (reverse-int 1234) 4321)
  
  (define (lychrel? n)
    (define (h n tries)
      (cond 
        [(> tries 50) #t]
        [(and (> tries 0) (is-palindrome? n)) #f]
        [else (h (+ n (reverse-int n)) (add1 tries))]))
    
    (h n 0))
  
  (check-equal? (lychrel? 47) #f)
  (check-equal? (lychrel? 349) #f)
  (check-equal? (lychrel? 196) #t)
  
  (length (filter lychrel? (range 1 10000))))

(provide solve)