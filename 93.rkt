#lang racket
(require "common.rkt")

; Find the set of four distinct digits, a < b < c < d, for which the longest
; set of consecutive positive integers, 1 to n, can be obtained using the
; operations +, -, *, /, giving your answer as a string: abcd.

(define (solve)
  
  (define (values-for-perm perm)
    (define ops (list + - * /))
    (define (all perm acc)
      (if (empty? perm) acc
          (map (lambda (x) 
                 (list 
                  (if (and (zero? (car perm)) (equal? / x)) 0
                      (all (cdr perm) (x acc (car perm))))
                  (if (and (zero? acc) (equal? / x)) 0
                      (all (cdr perm) (x (car perm) acc))))) ops)))
    (remove-duplicates
     (filter (lambda (x) (and (>= x 0) (= x (round x))))
             (flatten
              (all (cdr perm) (car perm))))))
  
  (define (longest-increasing lst)
    (define (h lst last curr acc)
      (cond [(empty? lst) acc]
            [(equal? (add1 last) (car lst))
             (h (cdr lst) (car lst) (add1 curr) (max acc (add1 curr)))]
            [else acc]))
    (h (cdr lst) (car lst) 1 1))
  
  (list->int
   (caar
    (sort
     (map (lambda (x)
            (define produced
              (sort 
               (remove-duplicates 
                (flatten 
                 (map values-for-perm 
                      (permutations x)))) <))
            (list x (longest-increasing produced) produced))
          (combinations (build-list 10 values) 4))
     (lambda (x y) (> (second x) (second y)))))))

(provide solve)