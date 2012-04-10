#lang racket
(require "common.rkt")

; Investigate sets with a special subset sum property.

; This problem was probably supposed to be harder than this:

(define (solve)
  (define (glue x) (list->int (flatten (map int->list x))))
  (glue (cons 20 (map (lambda (x) (+ x 20)) '(11 18 19 20 22 25)))))

(provide solve)