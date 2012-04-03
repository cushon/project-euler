#lang racket
(require "common.rkt")

; Find the minimal path sum in a matrix from the left column to the right
; column.

(define (solve)
  
  (define matrix
    (map (lambda (x) (map string->number (csv->lst x)))
         (file->lines "82.txt")))
  
  (define slices (apply zip matrix))
  
  (define (stomp a b)
    (define (h a b acc)
      (if (empty? a) (reverse acc)
          (h (cdr a) (cdr b)
             (cons 
              (if (cons? acc)
                  (min (+ (car a) (car b))
                       (+ (car a) (car acc)))
                  (+ (car a) (car b)))
              acc))))
    (h a b '()))
  
  (define (best-stomp a b)
    (map (lambda (x) (min (first x) (second x)))
         (zip (stomp a b) (reverse (stomp (reverse a) (reverse b))))))
  
  (define (solve slices)
    (if (empty? (cdr slices)) (car slices)
        (solve (cons (best-stomp (second slices)
                                 (first slices))
                     (cddr slices)))))
  
  (apply min (solve slices)))

(provide solve)