#lang racket
(require "common.rkt")

; Find all 0-9 pandigital numbers where digits n through n+2 are divisible by
; the n-th prime.

(define (solve)
  
  (define (multiples-upto m n)
    (map (lambda (x) (* m x)) (range 1 (quotient n m))))
  
  (define (zero-pad x n)
    (if (>= (length x) n) x
        (cons 0 (zero-pad x (sub1 n)))))
  
  (define (cands m)
    (map (lambda (x) (zero-pad x 3)) (map int->list (multiples-upto m 1000))))
  
  (define (possible n poss)
    (if (empty? n)
        poss
        (local ((define t (make-hash)))
          (for-each 
           (lambda (x) (hash-set! t (take x 2) (cons (drop x 2) (hash-ref t (take x 2) '()))))
           poss)
          (possible
           (cdr n)
           (foldr append '()
                  (map
                   (lambda (x)
                     (local ((define r (hash-ref t (take-right x 2) '())))
                       (if (empty? r) r (map (lambda (z) (append x z)) r))))
                   (cands (car n))))))))
  
  (define (missing n)
    (define (helper ex lst)
      (cond [(empty? lst) #f]
            [(= ex (car lst)) (helper (add1 (car lst)) (cdr lst))]
            [else ex]))
    (helper 0 (sort n <)))
  
  (foldl + 0
         (filter pandigital?
                 (map (lambda (x) (list->int (cons (missing x) x)))
                      (filter (lambda (x) (and (= (length x) 9)
                                               (pandigital? (list->int x))))
                              (possible '(13 11 7 5 3 2) (cands 17)))))))

(provide solve)