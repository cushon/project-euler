#lang racket
(require "common.rkt")

; Find the number of pairs of 6-sided cubes that can be arranged to display all
; square numbers below one hundred.

(define (solve)
  
  (define pairs '((0 1)
                  (0 4)
                  (0 6)
                  (1 6)
                  (2 5)
                  (3 6)
                  (4 6)
                  (8 1)))
  
  (define (try-all lst)
    (define (helper lst set-one set-two acc)
      (if (empty? lst) 
          (if (and (<= (length (remove-duplicates set-one)) 6)
                   (<= (length (remove-duplicates set-two)) 6))
              (cons
               (list (sort (remove-duplicates set-one) <) (sort (remove-duplicates set-two) <))
               acc)
              acc)
          (let ((acc (helper (cdr lst)
                             (cons (first (car lst)) set-one)
                             (cons (second (car lst)) set-two)
                             acc)))
            (helper (cdr lst)
                    (cons (first (car lst)) set-two)
                    (cons (second (car lst)) set-one)
                    acc))))
    (remove-duplicates (helper lst '() '() '())))
  
  (define (augmentations lst upto)
    (define cands (filter (lambda (x) (not (member x lst))) (build-list 10 values)))
    (define combs (combinations cands (- upto (length lst))))
    (map (lambda (x) (append lst x)) combs))
  
  (define (double-augment a b upto)
    (cartesian-product (augmentations a upto)
                       (augmentations b upto)))
  
  (define (xor a b) (and (or a b) (or (not a) (not b))))
  
  (define (extras lst)
    (if (xor (member 6 lst) (member 9 lst))
        (cond
          [(member 6 lst)
           (list lst (map (lambda (x) (if (= x 6) 9 x)) lst))]
          [(member 9 lst)
           (list lst (map (lambda (x) (if (= x 9) 6 x)) lst))])
        (list lst)))
  
  (length
   (remove-duplicates
    (map (lambda (x)
           (define a (sort (first x) <))
           (define b (sort (second x) <))
           (if (< (list->int a) (list->int b))
               (list a b)
               (list b a)))
         (reverse
          (foldl (lambda (x y) (append (reverse x) y)) '()
                 (map (lambda (x) (cartesian-product (extras (first x))
                                                     (extras (second x))))
                      (remove-duplicates
                       (reverse
                        (foldl (lambda (x y) (append (reverse x) y)) '()
                               (map
                                (lambda (x) (double-augment (first x)
                                                            (second x) 6))
                                (try-all pairs))))))))))))

(provide solve)