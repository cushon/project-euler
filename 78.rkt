#lang racket

; Find the least value of n for which the partition function P at n is
; divisible by one million.

; MacMahon's recurrence is used to calculate the partition function.

(define (solve)
  
  (define (pentagonal n)
    ((lambda (n) (/ (- (* 3 n n) n) 2))
     (if (even? n) (/ n 2) (- (/ (sub1 n) 2)))))
  
  (define (pentagonal-upto n)
    (define (h i)
      (local ((define p (pentagonal i)))
        (if (> p n) '() (cons p (h (add1 i))))))
    (h 1))
  
  (define seen (make-hash))
  
  (define (partition n)
    (define (positive-sign? x) (even? (quotient (if (even? x) (sub1 x) x) 2)))
    (define (helper n i ps acc)
      (cond [(= n 0) 1]
            [(= n 1) 1]
            [(= n 2) 2]
            [(= n 3) 3]
            [(= n 4) 5]
            [(empty? ps) acc]
            [else (helper 
                   n
                   (add1 i)
                   (cdr ps)
                   ((if (positive-sign? i) + -) acc (partition (- n (car ps)))))]))
    (hash-ref seen n
              (lambda ()
                (local ((define result 
                          (helper n 1 (cdr (pentagonal-upto n)) 0)))
                  (hash-set! seen n result)
                  result))))
  
  (define (test n)
    (if (zero? (remainder (partition n) 1000000)) n
        (test (add1 n))))
  
  (test 1))
  
(provide solve)