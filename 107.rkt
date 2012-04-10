#lang racket
(require "common.rkt")

; Find a MST of the given graph.

; Prim's.

(define (solve)
  
  (define (get-edges lst)
    (define (helper from to lst acc)
      (cond [(empty? lst) acc]
            [(equal? (car lst) "-") 
             (helper from (add1 to) (cdr lst) acc)]
            [else
             (helper from (add1 to) (cdr lst)
                     (cons (list from to (string->number (car lst)))
                           acc))]))
    (define (helper2 from lst acc)
      (if (empty? lst) (reverse acc)
          (helper2 (add1 from) (cdr lst)
                   (append (helper from 0 (car lst) '())
                           acc))))
    (remove-duplicates
     (map (lambda (x)
            (list (min (first x) (second x))
                  (max (first x) (second x))
                  (third x)))
          (helper2 0 lst '()))))
  
  (define graph
    (get-edges
     (map
      csv->lst
      (file->lines "107.txt"))))
  
  (define (get-min lst fn)
    (define (helper lst curr acc)
      (cond [(empty? lst) (list curr acc)]
            [(fn (car lst) curr)
             (helper (cdr lst) (car lst) (cons curr acc))]
            [else (helper (cdr lst) curr (cons (car lst) acc))]))
    (cond [(empty? lst) #f]
          [(empty? (cdr lst)) (list (car lst) '())]
          [else (helper (cdr lst) (car lst) '())]))
  
  (define (xor a b)
    (or (and a (not b))
        (and b (not a))))
  
  (define (prim edges)
    (define in-tree (make-hash))
    
    (define (primh edges acc)
      (if (empty? edges) acc
          (local ((define new-min 
                    (get-min 
                     (filter (lambda (x)
                               (xor (hash-has-key? in-tree (first x))
                                    (hash-has-key? in-tree (second x))))
                             edges)
                     (lambda (x y) (< (third x) (third y))))))
            (hash-set! in-tree (first (first new-min)) #t)
            (hash-set! in-tree (second (first new-min)) #t)
            (primh
             (filter (lambda (x)
                       (or (not (hash-has-key? in-tree (first x)))
                           (not (hash-has-key? in-tree (second x)))))
                     edges)
             (cons (first new-min) acc)))))
    
    (define first-min (get-min edges
                               (lambda (x y) (< (third x) (third y)))))
    
    (hash-set! in-tree (first (first first-min)) #t)
    
    (primh edges '()))
  
  (define (cost graph)
    (- (foldl + 0 (map third graph))
       (foldl + 0 (map third (prim graph)))))
  
  (cost graph))

(provide solve)