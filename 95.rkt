#lang racket
(require "common.rkt")

; Find the smallest member of the longest amicable chain with no element
; exceeding one million.

(define (solve)
  
  (define memo-is-prime? (memo-factory is-prime?))
  
  (define (build-primes cap)
    (define (helper n)
      (if (= n cap) '()
          (if (memo-is-prime? n)
              (cons n (helper (add1 n)))
              (helper (add1 n)))))
    (helper 2))
  
  (define primelist (build-primes 100000))
  
  (define seenf (make-hash))
  
  (define (fast-prime-factor n)
    (define (take-out n f)
      (if (zero? (remainder n f)) (take-out (quotient n f) f) n))
    (define (h n ps max)
      (hash-ref
       seenf n
       (lambda ()
         (let ((result
                (cond 
                  [(> (car ps) (min max n)) 
                   (if (memo-is-prime? n) (list n) '())]
                  [(zero? (modulo n (car ps)))
                   (cons (car ps) (h (take-out n (car ps))
                                     (cdr ps)
                                     max))]
                  [else (h n (cdr ps) max)])))
           (hash-set! seenf n result)
           result))))
    (h n primelist (sqrt n)))
  
  (define (all-facts n)
    (define (many n x)
      (define (h n x count)
        (if (not (zero? (remainder n x))) count
            (h (quotient n x) x (add1 count))))
      (h n x 0))
    (map (lambda (x) (list x (many n x))) (fast-prime-factor n)))
  
  (define (sum-facts facts)
    (define (h facts)
      (if (empty? facts) 1
          (local ((define factor (first (car facts)))
                  (define times (second (car facts)))
                  (define rest (h (cdr facts))))
            (foldl + 0
                   (map 
                    (lambda (x) (* (expt factor x) rest))
                    (build-list (add1 times) values))))))
    (- (h facts) (foldl * 1 (map (lambda (x) (apply expt x)) facts))))
  
  (define (next n) (sum-facts (all-facts n)))
  
  (define (chain-lengths n)
    (define travelled (make-hash))
    (define (helper n length first)
      (cond [(zero? n) '()]
            [(> n 1000000) '()]
            [(hash-has-key? travelled n)
             (if (hash-ref travelled n)
                 (hash-keys travelled)
                 '())]
            [else
             (hash-set! travelled n first)
             (helper (next n) (add1 length) #f)]))
    (helper n 0 #t))
  
  (define lengthse (make-hash))
  
  (define (solve cap)
    (define (helper n)
      (if (> n cap)
          (local
            ((define resultse (sort (hash->list lengthse)
                                    (lambda (x y) (> (cdr x) (cdr y))))))
            (take resultse (cdr (car resultse))))
          (if (hash-has-key? lengthse n)
              (helper (add1 n))
              (local
                ((define keys (chain-lengths n)))
                (for-each
                 (lambda (x) (hash-set! lengthse x (length keys)))
                 keys)
                (helper (add1 n))))))
    (helper 1))
  
  (apply min (map car (solve 1000000))))

(provide solve)