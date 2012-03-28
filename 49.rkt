#lang racket
(require "common.rkt")

; Find an increasing sequence of 4 digit primes that are permutations of
; each other.

(define (solve)
  
  (define t (make-hash))
  
  (define (key x) (sort (int->list x) <))
  
  (for-each
   (lambda (x) (hash-set! t (key x) (cons x (hash-ref t (key x) '()))))
   (filter is-prime? (range 999 10000)))
  
  (define (3s l)
    (match l
      [(list a b c _ ...) (cons (list a b c) (3s (cdr l)))]
      [else '()]))
  
  ; Assume we only need to inspect adjacent permutations - which, as it happens,
  ; we do.
  
  (list->int
   (flatten
    (map int->list
         (first
          (filter (lambda (x) (match x [(list a b c) (= (- b a) (- c b))]))
                  (foldr append '() 
                         (map (lambda (x) (3s (sort x <)))
                              (hash-values t)))))))))

(provide solve)