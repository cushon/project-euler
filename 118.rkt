#lang racket
(require "common.rkt")

; Find how many distinct sets containing each of the digits one through nine
; exactly once contain only prime elements.

(define (solve)
  
  (define is-prime-mem (memo-factory is-prime?))
  
  (define (proc-prefixes cand prefix acc all)
    (cond [(empty? cand)
           (if (is-prime? prefix)
               (h cand (cons prefix acc) all)
               all)]
          [(is-prime? prefix)
           (proc-prefixes (cdr cand)
                          (+ (car cand) (* 10 prefix))
                          acc
                          (h cand (cons prefix acc) all))]
          [else 
           (proc-prefixes
            (cdr cand) 
            (+ (car cand) (* 10 prefix)) 
            acc 
            all)]))
  
  (define (h cand acc all)
    (if (empty? cand) (cons (sort acc <) all)
        (proc-prefixes cand 0 acc all)))
  
  (define (find-subsets x) (h x '() '()))
  
  (length (remove-duplicates 
           (foldl append '() 
                  (map find-subsets (permutations (range 1 9)))))))

(provide solve)