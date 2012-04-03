#lang racket
(require "common.rkt")

; Find the minimal path sum in a matrix from the top left to the
; bottom right by only moving right and down.

(define (solve)
  
  (define matrix
    (map (lambda (x) (map string->number (csv->lst x)))
         (file->lines "81.txt")))
  
  (define (chop lsts n)
    (define (h lsts procd acc n)
      (if (or (zero? n) (empty? lsts))
          (list
           (reverse acc)
           (append (reverse procd) lsts))
          (h (cdr lsts) 
             (if (empty? (cdar lsts)) procd (cons (cdar lsts) procd))
             (cons (caar lsts) acc) (sub1 n))))
    (h lsts '() '() n))
  
  (define (slice lsts)
    (define (h lsts n)
      (if (empty? lsts) '()
          (local ((define layer (chop lsts n)))
            (cons (first layer) (h (second layer) (add1 n))))))
    (h lsts 1))
  
  (define slices (slice matrix))
  
  (define (push a b)
    (stomp (append (cons (car a) a) (list (last a))) b))
  
  (define (stomp a b)
    (if (empty? b) '()
        (cons (+ (car b) (min (first a) (second a)))
              (stomp (cdr a) (cdr b)))))
  
  (define (stompall lst)
    (cond [(empty? (cdr lst)) lst]
          [else 
           (stompall
            (cons (
                   (if (> (length (first lst)) (length (second lst))) stomp push)
                   (first lst) (second lst)) (cddr lst)))]))
  
  (caar (stompall slices)))

(provide solve)