#lang racket

; Find the sum of the products of each name in the list multiplied by its alphabetical
; value.

(define (solve)
  (define (alphanum n)
    (add1 (- (char->integer n) (char->integer #\A))))
  (define (process s) 
    (define (helper lst last)
      (cond [(empty? lst) (list (reverse last))]
            [(char=? #\" (car lst)) (helper (cdr lst) last)]
            [(char=? #\, (car lst)) (cons (reverse last) (helper (cdr lst) '()))]
            [else (helper (cdr lst) (cons (car lst) last))]))
    (define (weight word)
      (foldr + 0 (map alphanum (string->list word))))
    (define (score lst i)
      (if (empty? lst) lst
          (cons (* (weight (car lst)) i) (score (cdr lst) (add1 i)))))
    (foldl + 0 
           (score (sort (map list->string (helper (string->list s) '()))
                        string<?) 1)))
  (process (file->string "22.txt")))

(provide solve)