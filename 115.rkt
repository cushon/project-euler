#lang racket
(require "common.rkt")

; Find the first row length that can be filled with blocks more than one
; million ways, assuming blocks have length >= 3, and a 1 unit gap is left
; between blocks.

(define (solve)
  
  (define seen (make-hash))
  
  (define (fill-h m k)
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
                                         (+ 1 (fill-h m (sub1 start-index))))
                                       (range 0 (- k block-len)))))
                             (range m k)))))
                (hash-set! seen (list m k) result)
                result)))]))
  
  (define (fill m n) (add1 (fill-h m n)))
  
  (define (solve)
    (define (helper i)
      (if (> (fill 50 i) 1000000) i
          (helper (add1 i))))
    (helper 1))
  
  (solve))

(provide solve)