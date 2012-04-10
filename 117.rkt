#lang racket
(require "common.rkt")

; Find how many ways a rectangle of length 50 be filled with blocks of length
; 2, 3, or 5.

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
                             (lambda (block-len)
                               (foldl + 0
                                      (map
                                       (lambda (start-index)
                                         (+ 1 (fill m start-index)))
                                       (range 0 (- k block-len)))))
                             (range 2 4)))))
                (hash-set! seen (list m k) result)
                result)))]))
  
  (add1 (fill 2 50)))

(provide solve)