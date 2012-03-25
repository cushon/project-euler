#lang racket
(require "common.rkt")

; Find the sum of all the positive integers which cannot be written as the sum
; of two abundant numbers.

(define (solve)
  
  (define (proper-divisors n)
    (define lst '())
    (define (helper n acc)
      (if (empty? n) (set! lst (cons acc lst))
          (begin
            (helper (cdr n) acc)
            (helper (cdr n) (* (car n) acc)))))
    (helper (prime-factor n) 1)
    (reverse (cdr (sort (remove-duplicates lst) >))))
  
  (define (abundant? n)
    (< n (foldr + 0 (proper-divisors n))))
  
  (define (sum n)
    (define (helper i lst)
      (cond [(> i n) 0]
            [(= (car lst) i) (helper (add1 i) (cdr lst))]
            [else (+ i (helper (add1 i) lst))]))
    (helper 
     1
     ((lambda (l) (sort (remove-duplicates 
                         (foldr append '() 
                                (map (lambda (x) 
                                       (map (lambda (y) (+ y x)) l)) l))) <))
      (filter abundant? (build-list 28123 add1)))))
  
  (sum 28123))

(provide solve)