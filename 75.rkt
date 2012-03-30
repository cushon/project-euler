#lang racket

; Find the number of perimeters <= 1500000 for which only one integer sided
; right angle triangles can be formed.

(define (solve)
  
  (define cap 1500000)
  
  (define (get-triples m n) 
    (list (- (* m m) (* n n)) (* 2 m n) (+ (* m m) (* n n))))
  
  (define (gen m n)
    (cond [(> (foldl + 0 (get-triples m 1)) cap) '()]
          [(not (> m n)) (gen (add1 m) 1)]
          [(> (foldl + 0 (get-triples m n)) cap) (gen (add1 m) 1)]
          [else (cons (list m n) (gen m (add1 n)))]))
  
  (define l (filter (lambda (x) (and (= 1 (remainder (+ (first x) (second x)) 2))
                                     (= 1 (gcd (first x) (second x)))))
                    (gen 2 1)))
  
  (define seen (make-hash))
  
  (define (set m n i)
    (define sum (* i (foldl + 0 (get-triples m n))))
    (if (> sum cap) '()
        (cons (map (lambda (x) (* x i))
                   (get-triples m n))
              (set m n (add1 i)))))
  
  (define all
    (foldl append '()
           (map (lambda (x) (set (first x) (second x) 1)) l)))
  
  
  (for-each (lambda (x) (hash-set! seen (foldr + 0 x) 
                                   (add1 (hash-ref seen (foldr + 0 x) 0))))
            all)
  
  (length (filter (lambda (x) (= x 1)) (hash-values seen))))

(provide solve)