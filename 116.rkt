#lang racket
(require "common.rkt")

; Find how many ways a rectangle of length 50 be filled with blocks of length
; 2, 3, xor 5, leaving a 1 unit gap beween blocks.

(define (solve)
  
  (define seen (make-hash))
  
  (define (fill m k)
    (cond [(= m k) 1]
          [(< k m) 0]
          [else
           (hash-ref 
            seen (list m k)
            (lambda()
              (let ((result
                     (foldl + 0
                            (map
                             (lambda (start-index)
                               (+ 1 (fill m start-index)))
                             (range 0 (- k m))))))
                (hash-set! seen (list m k) result)
                result)))]))
  
  (+
   (fill 2 50)
   (fill 3 50)
   (fill 4 50)))

(provide solve)