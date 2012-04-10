#lang racket
(require "common.rkt")

; Find how many triangles in the text file contain the origin.

(define (solve)
  
  (define (make-tri x)
    (match x 
      [(list i j k l m n)
       (list (list i j k l) (list k l m n) (list m n i j))]))
  
  (define triangles
    (map make-tri
         (map (lambda (x) (map string->number (csv->lst x)))
              (file->lines "102.txt"))))
  
  (define (y-int a b c d)
    (if (zero? (- c a)) (if (zero? c) c #f)
        (+ b (* (/ (- d b) (- c a)) (- 0 a)))))
  
  (define (x-int a b c d)
    (if (zero? (- d b)) (if (zero? d) d #f)
        (+ a (* (/ (- c a) (- d b)) (- 0 b)))))
  
  (define (between? x a b)
    (and (number? x) (number? b) (number? x)
         (>= x (min a b)) (<= x (max a b))
         x))
  
  (define (straddle? a b)
    (and (<= (min a b) 0) (>= (max a b) 0)))
  
  (define (contains-origin? x)
    (define (helper fn)
      (define pts 
        (remove-duplicates
         (filter number?
                 (map (lambda (x) (apply between? (fn x))) x))))
      (and (= 2 (length pts)) (apply straddle? pts)))   
    (and (helper (lambda (x) (list (apply x-int x) (first x) (third x))))
         (helper (lambda (x) (list (apply y-int x) (second x) (fourth x))))))
  
  (length (filter contains-origin? triangles)))

(provide solve)