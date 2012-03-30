#lang racket
(require rackunit)

; Find the sum of the only ordered set of six cyclic 4-digit numbers for which
; each polygonal type: triangle, square, pentagonal, hexagonal, heptagonal, and
; octagonal, is represented by a different number in the set.

(define (solve)
  
  (define (index-of lst i)
    (car (drop lst i)))
  
  (check-equal? (index-of '(1 2 3) 0) 1)
  (check-equal? (index-of '(1 2 3) 1) 2)
  
  (define (contains? lst n)
    (and (cons? lst)
         (or (equal? n (car lst)) (contains? (cdr lst) n))))
  
  (check-equal? (contains? '(1 2 3) 1) #t)
  (check-equal? (contains? '(1 2 3) 4) #f)
  (check-equal? (contains? '() 1) #f)
  
  (define (splits lst)
    (define (h a b)
      (if (empty? b) '()
          (cons (list (car b) (append (reverse a) (cdr b)))
                (h (cons (car b) a) (cdr b)))))
    (h '() lst))
  
  (check-equal? (splits '(1 2 3)) '((1 (2 3)) (2 (1 3)) (3 (1 2))))
  (check-equal? (splits '(1)) '((1 ())))
  (check-equal? (splits '()) '())
  
  (define (zipn lst)
    (if (foldl (lambda (x y) (or (empty? x) y)) #f lst) '()
        (cons (map car lst) (zipn (map cdr lst)))))
  
  (define (zip a b) (zipn (list a b)))
  
  (check-equal? (zip '(1 2 3) '(4 5 6)) '((1 4) (2 5) (3 6)))
  
  (define (gen f n)
    (match f
      [3 (* n (/ (add1 n) 2))]
      [4 (* n n)]
      [5 (* n (/ (- (* n 3) 1) 2))]
      [6 (* n (- (* n 2) 1))]
      [7 (* n (/ (- (* n 5) 3) 2))]
      [8 (* n (- (* n 3) 2))]))
  
  (check-equal? (map (lambda (x) (gen 3 x)) (build-list 5 add1)) '(1 3 6 10 15))
  (check-equal? (map (lambda (x) (gen 4 x)) (build-list 5 add1)) '(1 4 9 16 25))
  (check-equal? (map (lambda (x) (gen 5 x)) (build-list 5 add1)) '(1 5 12 22 35))
  (check-equal? (map (lambda (x) (gen 6 x)) (build-list 5 add1)) '(1 6 15 28 45))
  (check-equal? (map (lambda (x) (gen 7 x)) (build-list 5 add1)) '(1 7 18 34 55))
  (check-equal? (map (lambda (x) (gen 8 x)) (build-list 5 add1)) '(1 8 21 40 65))
  
  (define (get f max)
    (define (h i)
      (define next (gen f i))
      (cond [(> next max) '()]
            [(and (> next 1000)
                  (not (= (remainder next 100) 0)))
             (cons next (h (add1 i)))]
            [else (h (add1 i))]))
    (h 1))
  
  
  (define (chopar n) (quotient n 100))
  (define (chopdr n) (remainder n 100))
  
  (check-equal? (chopar 1234) 12)
  (check-equal? (chopdr 1234) 34)
  
  (define nodes (make-hash))
  
  (for-each
   (lambda (x)
     (hash-set! nodes x
                ((lambda (t)
                   (for-each (lambda (k) (hash-set! t (chopar k) (cons (chopdr k) (hash-ref! t (chopar k) '()))))
                             (get x 10000))
                   t)
                 (make-hash))))
   (build-list 6 (lambda (x) (+ x 3))))
  
  
  (define (solve f fs choices path track)
    (define (next f1 f2 first)
      (filter
       (lambda (first) (hash-has-key? (hash-ref nodes f2) first))
       (hash-ref (hash-ref nodes f1) first '())))
    (cond [(empty? choices) #f]
          [(empty? fs)
           (if (contains? (next f (last track) (car choices)) (last path))
               (reverse (zip (cons (car choices) path) (cons f track)))
               (solve f fs (cdr choices) path track))]
          [else (or
                 (foldl (lambda (try y)
                          (or y
                              (solve (first try) 
                                     (splits (second try)) 
                                     (next f (first try) (car choices))
                                     (cons (car choices) path) (cons f track))))
                        #f
                        fs)
                 (solve f fs (cdr choices) path track))]))
  
  (define (build lst)
    (define pieces (map first lst))
    (define (join a b) (+ (* 100 a) b))
    (define (h lst)
      (if (empty? (cdr lst)) 
          (list (join (car lst) (car pieces)))
          (cons (join (car lst) (cadr lst)) (h (cdr lst)))))
    (h pieces))
  
  (foldr + 0 (build 
              (solve 3 
                     (splits '(4 5 6 7 8))
                     (hash-keys (hash-ref nodes 3)) 
                     '()
                     '()))))

(provide solve)