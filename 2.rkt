#lang racket

; Find the sum of even fibonnaci numbers up to 4 million.

(define (solve)
  (define (fib-sum upto filter)
    (define (h i j acc)
      (if (> i upto) acc
          (h j (+ i j) (if (filter i) (+ i acc) acc))))
    (h 1 2 0))
  (fib-sum 4000000 even?))

(provide solve)
