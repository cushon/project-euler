#lang racket
(require "common.rkt")

; Find the least value of n for which the number of distinct integer solutions
; to 1/x + 1/y = 1/n exceeds one thousand.

(define (solve)
  
  (define (group lst)
    (define (helper lst last last-len)
      (cond [(empty? lst) (list (list last-len last))]
            [(= (car lst) last) (helper (cdr lst) last (add1 last-len))]
            [else (cons (list last-len last)
                        (helper (cdr lst) (car lst) 1))]))
    (helper (cdr lst) (car lst) 1))
  
  (define (prime-factor-sq x)
    (define ps (prime-factor x))
    (sort (append ps ps) <))
  
  (define (num k)
    (ceiling 
     (/ (foldl * 1 
               (map add1 (map first (group (prime-factor-sq k)))))
        2)))
  
  (define (find cap)
    (define (helper trial)
      (define r (num trial))
      (if (> r cap)
          trial
          (helper (add1 trial))))
    (helper 1))
  
  (find 1000))

(provide solve)