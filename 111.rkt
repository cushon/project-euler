#lang racket
(require "common.rkt")

; Find the sum of all 10 digits primes that have the maximal occurence of some
; digit.

(define (solve)
  
  (define (seen lst)
    (define (h last last-c lst)
      (cond [(and (empty? lst) (> last 9)) '()]
            [(empty? lst) (cons last-c (h (add1 last) 0 lst))]
            [(= last (car lst))
             (h last (add1 last-c) (cdr lst))]
            [else
             (cons last-c (h (add1 last) 0 lst))]))
    (h 0 0 lst))
  
  (define (h i seen prime acc)
    (if (empty? seen) '()
        (begin
          (cond [(= (car seen) (first (car acc)))
                 (cons (list (first (car acc))
                             (add1 (second (car acc)))
                             (+ prime (third (car acc))))
                       (h (add1 i) (cdr seen) prime (cdr acc)))]
                [(> (car seen) (first (car acc)))
                 (cons (list (car seen)
                             1
                             prime)
                       (h (add1 i) (cdr seen) prime (cdr acc)))]
                [else
                 (cons (car acc) (h (add1 i) (cdr seen) prime (cdr acc)))]))))
  
  (define (tally current max acc)
    (cond [(> current max) (foldl + 0 (map third acc))]
          [(not (is-prime? current)) (tally (+ current 2) max acc)]
          [else
           (tally (+ current 2) max
                  (h 0 (seen (sort (int->list current) <)) current acc))]))
  
  (define (replacements orig len acc many opts all)
    (cond
      [(empty? orig) (cons (reverse acc) all)]
      [(zero? many) (cons (append (reverse acc) orig) all)]
      [else
       (let ((r (if (> len many)
                    (replacements 
                     (cdr orig) (sub1 len) (cons (car orig) acc) many opts all)
                    all)))
         (foldl (lambda (x y)
                  (replacements
                   (cdr orig) (sub1 len) (cons x acc) (sub1 many) opts y))
                r
                opts))]))
  
  (define (get-longest-for digit len)
    (define (h reps)
      (define r 
        (filter
         (lambda (x) (and (= len (length (int->list (list->int x))))
                          (is-prime? (list->int x))))
         (replacements 
          (build-list len (lambda (x) digit)) len '() reps (range 0 9) '())))
      (if (cons? r) (map list->int r) (h (add1 reps))))
    (h 1))
  
  (foldl + 0
         (flatten
          (map
           (lambda (x) (get-longest-for x 10))
           (range 0 9)))))

(provide solve)