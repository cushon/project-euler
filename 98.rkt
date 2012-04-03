#lang racket
(require "common.rkt")

; Find the largest square number formed by any member of a pair of anagrams
; where the same digital substitution for each word is a square number.

(define (solve)
  
  (define (custmax lst fn)
    (cond [(empty? lst) #f]
          [(empty? (cdr lst)) (car lst)]
          [else
           (foldl (lambda (x y) (if (fn x y) x y))
                  (car lst)
                  (cdr lst))]))
  
  (define words (csv->lst (file->string "98.txt")))
  
  (define anagrams-table (make-hash))
  
  (for-each
   (lambda (x) 
     (define key (sort (string->list x) char<?))
     (hash-set! anagrams-table
                key
                (cons x (hash-ref anagrams-table key '()))))
   words)
  
  (define anagrams
    (filter (lambda (x) (> (length x) 1))
            (hash-values anagrams-table)))
  
  (define maxlen
    (string-length (first 
                    (custmax
                     anagrams
                     (lambda (x y) (apply < (map string-length x)))))))
  
  (define (perfects cap)
    (define (helper n)
      (if (> (* n n) cap) '()
          (cons (* n n) (helper (add1 n)))))
    (helper 1))
  
  (define perfects-by-len (make-hash))
  
  (define (num-len n) (length (int->list n)))
  
  (define perfect-anagrams-tab (make-hash))
  
  (for-each
   (lambda (perfect)
     (hash-set! perfect-anagrams-tab
                (sort (int->list perfect) <)
                (cons perfect
                      (hash-ref perfect-anagrams-tab
                                (sort (int->list perfect) <)
                                '()))))
   (perfects (expt 10 maxlen)))
  
  
  (define perfect-anagrams
    (filter (lambda (x) (> (length x) 1))
            (hash-values perfect-anagrams-tab)))
  
  (define alla (flatten perfect-anagrams))
  
  (define (switch a b)
    (define (norm x)
      (sort (zip (string->list x)
                 (build-list (string-length x) values))
            (lambda (x y) (char<? (first x) (first y)))))
    (define order
      (map second
           (sort
            (map (lambda (x) (list (second (first x))
                                   (second (second x))))
                 (zip (norm a) (norm b)))
            (lambda (x y) (< (first x) (first y))))))
    (define (helper order digit-vector)
      (if (empty? order) '()
          (cons (vector-ref digit-vector (car order))
                (helper (cdr order) digit-vector))))
    (lambda (x)
      (helper order (list->vector (int->list x)))))
  
  
  (define (find-of-len cap)
    (filter 
     (lambda (x) (not (empty? (second x))))
     (map
      (lambda (pair)
        (list pair
              (filter
               (lambda (x)
                 (define res (list->int ((apply switch pair) x)))
                 (and (exact? (sqrt res)) (= (num-len x) (num-len res))))
               (filter (lambda (x) 
                         (and (= (num-len x) cap)
                              (= (length (remove-duplicates (int->list x))) cap)))
                       alla))))
      (filter (lambda (x) (= (string-length (car x)) cap)) anagrams))))
  
  (apply max
         (map
          (lambda (x) (max (caadr x) (list->int ((apply switch (first x)) (caadr x)))))
          (foldl append '()
                 (filter cons? 
                         (map find-of-len (range 5 10)))))))

(provide solve)