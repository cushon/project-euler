#lang racket

; Find the number of characters saved by writing each of the given roman
; numerals in their minimal form.

(define (solve)
  
  (define (min-size num)
    (define weights '((1000 1) (900 2) (500 1) (400 2) (100 1) (90 2) (50 1)
                               (40 2) (10 1) (9 2) (5 1) (4 2) (1 1)))
    (define (helper n weights sum)
      (cond
        [(zero? n) sum]
        [(> (caar weights) n) (helper n (cdr weights) sum)]
        [(helper (remainder n (caar weights))
                 (cdr weights)
                 (+ (* (quotient n (caar weights)) (cadar weights)) sum))]))
    (helper num weights 0))
  
  (define (to-num str)
    (define (helper lst acc)
      (match lst
        [(? empty?) acc]
        [(list #\M     rest ...) (helper rest (+ acc 1000))]
        [(list #\C #\M rest ...) (helper rest (+ acc 900))]
        [(list #\C #\D rest ...) (helper rest (+ acc 400))]
        [(list #\D     rest ...) (helper rest (+ acc 500))]
        [(list #\C     rest ...) (helper rest (+ acc 100))]
        [(list #\X #\C rest ...) (helper rest (+ acc 90))]
        [(list #\L     rest ...) (helper rest (+ acc 50))]
        [(list #\X #\L rest ...) (helper rest (+ acc 40))]
        [(list #\X     rest ...) (helper rest (+ acc 10))]
        [(list #\I #\X rest ...) (helper rest (+ acc 9))]
        [(list #\V     rest ...) (helper rest (+ acc 5))]
        [(list #\I #\V rest ...) (helper rest (+ acc 4))]
        [(list #\I     rest ...) (helper rest (add1 acc))]))
    (helper (string->list str) 0))
  
  (foldl 
   + 0
   (map
    (lambda (x) (- (length (string->list x)) (min-size (to-num x))))
    (file->lines "89.txt"))))

(provide solve)