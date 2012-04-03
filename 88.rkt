#lang racket
(require "common.rkt")

; Find the sum of all the minimal product-sum numbers for 2 <= k <= 12000.

(define (solve)
  
  (define (factrange start n)
    (define (helper trial)
      (cond [(>= trial n) '()]
            [(zero? (remainder n trial))
             (cons trial (helper (add1 trial)))]
            [else (helper (add1 trial))]))
    (helper start))
  
  (define (facts n) (factrange 2 n))
  
  (define seen (make-hash))
  
  (define (all n)
    (define rset '())
    (define (allfcts low n acc)
      (if (zero? n)
          (set! rset (cons (reverse acc) rset))
          (local
            ((define fcts (factrange low n))
             (define result
               (if (empty? fcts)
                   (set! rset (cons (reverse (cons n acc)) rset))
                   (for-each (lambda (x) (allfcts x (quotient n x) (cons x acc)))
                             fcts))))
            result)))
    (allfcts 2 n '())
    rset)
  
  (define (k lst)
    (define p (foldl * 1 lst))
    (define s (foldl + 0 lst))
    (+ (length lst) (- p s)))
  
  (define mins (make-hash))
  
  (for-each
   (lambda (x)
     (define tk (k x))
     (define p (foldl * 1 x))
     (and (> (hash-ref mins tk (add1 p)) p) (hash-set! mins tk p)))
   (foldl append '() (map all (range 2 24000))))
  
  
  (foldl + 0
         (remove-duplicates
          (map cdr
               (filter
                (lambda (x) (and (>= (car x) 2) (<= (car x) 12000)))
                (hash->list mins))))))

(provide solve)