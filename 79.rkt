#lang racket
(require "common.rkt")

; Find the shortest integer that all the given integers are ordered
; subsequences of.

(define (solve)
  
  (define keys (map int->list (map string->number (file->lines "79.txt"))))
  (define cands (remove-duplicates (flatten keys)))
  
  (define (hash-map! h f)
    (hash-for-each h (lambda (x y) (hash-set! h x (f y)))))
  
  (define more (make-hash))
  
  (for-each (lambda (x) (hash-set! more x '())) cands)
  
  (define (add-more a b) (hash-set! more a (cons b (hash-ref more a))))
  
  (for-each 
   (lambda (x)
     (add-more (second x) (first x))
     (add-more (third x) (first x))
     (add-more (third x) (second x)))
   keys)
  
  (hash-map! more remove-duplicates)
  
  (define (helper cands)
    (define (excluder x)
      (lambda (lst) (filter (lambda (z) (not (= z x))) lst)))
    (if (empty? cands) '()
        (let ((next (first (filter (lambda (x) (empty? (hash-ref more x))) cands))))
          (hash-map! more (excluder next))
          (cons next (helper ((excluder next) cands))))))
  
  (list->int (helper (remove-duplicates (flatten keys)))))

(provide solve)