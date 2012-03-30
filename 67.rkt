#lang racket
(require rackunit "common.rkt")

; Find the maximum top to bottom sum in the given triangle.

(define (solve)
  
  (define triangle
    (map (lambda (x) (map string->number (tokenize x)))
         (file->lines "67.txt")))
  
  (define (max-sum triangle)
    (define (get-next a b)
      (if (empty? a) '()
          (cons (+ (first a) (max (first b) (second b)))
                (get-next (cdr a) (cdr b)))))
    (define (h row rows)
      (define next (car rows))
      (define rest (cdr rows))
      (cond [(empty? rest) (first (get-next next row))]
            [else (h (get-next next row) rest)]))
    (h (car (reverse triangle)) (cdr (reverse triangle))))
  
  (max-sum triangle))

(provide solve)