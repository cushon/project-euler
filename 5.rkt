#lang racket
(require "common.rkt")

(define (solve)
  (define (condense lst)
    (define (helper lst last seen)
      (cond [(empty? lst) (list (list last seen))]
            [(= (car lst) last) (helper (cdr lst) last (add1 seen))]
            [else (cons (list last seen) (helper (cdr lst) (car lst) 1))]))
    (helper lst (car lst) 0))
  (define (merge-factors a b)
    (cond [(and (empty? a) (empty? b)) '()]
          [(empty? a) b]
          [(empty? b) a]
          [(= (caar a) (caar b))
           (cons (list (caar a) (max (cadar a) (cadar b)))
                 (merge-factors (cdr a) (cdr b)))]
          [(< (caar a) (caar b)) 
           (cons (car a) (merge-factors (cdr a) b))]
          [else (cons (car b) (merge-factors a (cdr b)))]))
  (define (expand-out n)
    (foldr (lambda (x y) (* (expt (first x) (second x)) y)) 1 n))
  (expand-out (foldr merge-factors '() 
                     (map condense (map prime-factor (range 1 20))))))

(provide solve)