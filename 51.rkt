#lang racket
(require "common.rkt")

; Find the smallest prime which, by replacing part of the number (not
; necessarily adjacent digits) with the same digit, is part of an eight
; prime value family.

(define (solve)
  
  (define (templates lst)
    (define acc '())
    (define (helper lst tmp)
      (if (empty? lst) 
          (set! acc (cons (reverse tmp) acc))
          (begin
            (helper (cdr lst) (cons (car lst) tmp))
            (helper (cdr lst) (cons 'x tmp)))))
    (helper lst '())
    (drop-right (drop acc 1) 1))
  
  (define (search number for-len)
    (define (replace q t lst) (map (lambda (x) (if (equal? x q) t x)) lst))
    (define (helper ts)
      (or
       (empty? ts)
       (local
         ((define family 
            (filter (lambda (x) (and (is-prime? x) 
                                     (= (length (int->list x)) 
                                        (length (int->list number)))))
                    (map (lambda (rep) (list->int (replace 'x rep (car ts))))
                         (range 1 10)))))
         (if (= (length family) for-len)
             family
             (helper (cdr ts))))))
    (helper (templates (int->list number))))
  
  (define (solve k)
    (define (h n i)
      (if (not (is-prime? n))
          (h (add1 n) (add1 i))
          (local
            ((define result (search n k)))
            (if (boolean? result)
                (h (add1 n) (add1 i))
                result))))
    (h 1 0))
  
  (apply min (solve 8)))

(provide solve)