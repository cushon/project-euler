#lang racket
(require "common.rkt")

; Find all sets where no two subsets have the same sum, and subsets of greater
; size have greater sums.

(define (solve)
  
  (define (subsets lst)
    (define (helper lst acc)
      (if (empty? lst) (list (reverse acc))
          (append
           (helper (cdr lst) (cons (car lst) acc))
           (helper (cdr lst) acc))))
    (filter cons? (helper lst '())))
  
  (define (group lst)
    (define (helper curr lst acc)
      (cond [(empty? lst)
             (if (empty? acc) acc (list (reverse acc)))]
            [(= (length (car lst)) curr)
             (helper curr (cdr lst) (cons (car lst) acc))]
            [else
             (cons (reverse acc)
                   (helper (length (car lst)) (cdr lst) (list (car lst))))]))
    (helper (length (car lst)) lst '()))
  
  (define (increasing lst)
    (or (empty? lst) 
        (empty? (cdr lst))
        (and
         (< (second (car lst)) (first (car (cdr lst))))
         (increasing (cdr lst)))))
  
  (define (special? lst)
    (define ss (subsets lst))
    (and
     (= (length ss) 
        (length (remove-duplicates
                 (map (lambda (x) (foldl + 0 x))
                      ss))))
     (increasing
      (map (lambda (x)
             (define lens (map (lambda (x) (foldl + 0 x)) x))
             (list (apply min lens) (apply max lens)))
           (group (sort (subsets lst) (lambda (x y) (< (length x) (length y)))))))))
  
  (foldl 
   + 0 
   (flatten
    (filter special?
            (map
             (lambda (x) (map string->number (csv->lst x)))
             (file->lines "105.txt"))))))

(provide solve)