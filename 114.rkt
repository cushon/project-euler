#lang racket
(require "common.rkt")

; Find how many ways a row measuring fifty units in length be filled with
; blocks at least three units in length, leaving a 1 unit gap between
; blocks.

(define (solve)
  
  (define seen (make-hash))
  
  (hash-set! seen 3 1)
  
  (define (fills k)
    (if (< k 3) 0
        (hash-ref 
         seen k
         (lambda()
           (let ((result
                  (foldl + 0
                         (map
                          (lambda (block-len)
                            (foldl + 0
                                   (map
                                    (lambda (start-index)
                                      (+ 1 (fills (sub1 start-index))))
                                    (range 0 (- k block-len)))))
                          (range 3 k)))))
             (hash-set! seen k result)
             result)))))
  
  (add1 (fills 50)))

(provide solve)