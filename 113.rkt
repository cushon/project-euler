#lang racket
(require "common.rkt")

; Find the number of numbers below a googol that are not bouncy.

(define (solve)
  
  (define (for-len l)
    (+ 9
       (foldl 
        + 0
        (map 
         (lambda (num-digits)
           (let
               ((split-ways (choose (sub1 l) (sub1 num-digits)))
                (incr-digit-choices (if (= 10 num-digits) 0 (choose 9 num-digits)))
                (decr-digit-choices (choose 10 num-digits)))
             (* split-ways (+ incr-digit-choices decr-digit-choices))))
         (range 2 (min l 10))))))
  
  (define (b-for-len l) (- (* 9 (expt 10 (sub1 l))) (for-len l)))
  
  (- (sub1 (expt 10 100)) (foldl + 0 (map b-for-len (range 1 100)))))

(provide solve)