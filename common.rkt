#lang racket

(require rackunit)

(define (range a b)
  (build-list (add1 (- b a)) (lambda (x) (+ x a))))

(check-equal? (range 0 0) '(0))
(check-equal? (range 0 -1) '())
(check-equal? (range 0 4) '(0 1 2 3 4))
(check-equal? (range 5 8) '(5 6 7 8))

(provide range)

(define (cartesian-product a b)
  (reverse (foldl 
            append '() 
            (map (lambda (x) (reverse (map (lambda (y) (list x y)) b))) a))))

(check-equal? (cartesian-product '(1 2) '(1 2 3))
              '((1 1) (1 2) (1 3) (2 1) (2 2) (2 3)))

(provide cartesian-product)

(define (is-palindrome? n)
  (define (helper n acc)
    (if (> n 0)
        (helper (quotient n 10) (+ (remainder n 10) (* 10 acc)))
        acc))
  (equal? n (helper n 0)))

(check-equal? (is-palindrome? 123) #f)
(check-equal? (is-palindrome? 12345654321) #t)
(check-equal? (is-palindrome? 111) #t)
(check-equal? (is-palindrome? 1) #t)
(check-equal? (is-palindrome? 0) #t)

(provide is-palindrome?)

(define (prime-factor n)
  (define (helper i k)
    (cond [(>= i k) (list k)]
          [(zero? (remainder k i)) (append (prime-factor i) (helper i (/ k i)))]
          [else (helper (add1 i) k)]))
  (helper 2 n))

(check-equal? (prime-factor 360) '(2 2 2 3 3 5))

(provide prime-factor)

(define (int->list n)
  (define (helper n acc)
    (if (zero? n) acc
        (helper (quotient n 10) (cons (remainder n 10) acc))))
  (if (zero? n) '(0) (helper n '())))

(check-equal? (int->list 123000) '(1 2 3 0 0 0))
(check-equal? (int->list 0) '(0))

(provide int->list)

(define (list->int lst)
  (foldl (lambda (x y) (+ x (* y 10))) 0 lst))

(check-equal? (list->int '(1 2 3 0)) 1230)
(check-equal? (list->int '(0)) 0)
(check-equal? (list->int '()) 0)
(check-equal? (list->int '(0 0 0 1 2 3)) 123)

(provide list->int)

(define (is-prime? n)
  (define (test a b)
    (or (> a b) (and (not (zero? (remainder n a))) (test (+ 2 a) b))))
  (or (= n 2) 
      (and (> n 2)
           (not (zero? (remainder n 2)))
           (test 3 (sqrt n)))))

(check-equal? (is-prime? 1) #f)
(check-equal? (is-prime? 17) #t)
(check-equal? (is-prime? 360) #f)

(provide is-prime?)

(define (factorial n)
  (define (helper n acc)
    (cond [(zero? n) 1]
          [(= 1 n) acc]
          [else (helper (sub1 n) (* n acc))]))
  (helper n 1))

(check-equal? (factorial 1) 1)
(check-equal? (factorial 2) 2)
(check-equal? (factorial 3) 6)
(check-equal? (factorial 4) 24)
(check-equal? (factorial 0) 1)

(provide factorial)

(define (pan-helper? strict n)
  (define v (make-vector 10))
  (if strict (vector-set! v 0 #f) (void))
  (define (h n)
    (or (zero? n)
        (and (not (boolean? (vector-ref v (remainder n 10))))
             (begin
               (vector-set! v (remainder n 10) #f)
               (h (quotient n 10))))))
  (define (g n)
    (or (empty? n)
        (and (h (car n))
             (g (cdr n)))))
  (and (g n)
       (or (not strict)
           (and (not (vector-ref v 0))
                (foldl (lambda (x y) (and x y)) #t 
                       (map boolean? (cdr (vector->list v))))))))

(define (strictly-pandigital? . n) (pan-helper? #t n))

(check-equal? (strictly-pandigital? 123 456 7 8 9) #t)
(check-equal? (strictly-pandigital? 123 456 7 8) #f)
(check-equal? (strictly-pandigital? 123 456 7 8 8) #f)
(check-equal? (strictly-pandigital? 1230 456 7 8 9) #f)
(check-equal? (strictly-pandigital? 12300 456 7 8 8) #f)

(provide strictly-pandigital?)

(define (pandigital? . n) (pan-helper? #f n))

(check-equal? (pandigital? 123 456 7 8 9) #t)
(check-equal? (pandigital? 123 456 7 8) #t)
(check-equal? (pandigital? 123 456 7 8 8) #f)
(check-equal? (pandigital? 106357289) #t)
(check-equal? (pandigital? 1230 456 7 8 9) #t)
(check-equal? (pandigital? 12300 456 7 8 8) #f)

(provide pandigital?)

(define (csv->lst csv)
  (define (helper lst acc)
    (cond [(empty? lst) (if (empty? acc) acc (list (list->string (reverse acc))))]
          [(or (char-whitespace? (car lst))
               (char=? (car lst) #\"))
           (helper (cdr lst) acc)]
          [(char=? (car lst) #\,)
           (if (empty? acc)
               (helper (cdr lst) '())
               (cons (list->string (reverse acc))
                     (helper (cdr lst) '())))]
          [else (helper (cdr lst) (cons (car lst) acc))]))
  (helper (string->list csv) '()))

(check-equal? (csv->lst "\"asd\",\"dsa\"") '("asd" "dsa"))
(check-equal? (csv->lst "\"asd\",,\"\"dsa\",,,") '("asd" "dsa"))
(check-equal? (csv->lst "\"asd\"") '("asd"))

(provide csv->lst)

(define (memo-factory fn)
  (define t (make-hash))
  (lambda x 
    (hash-ref t x
              (lambda ()
                (define result (apply fn x))
                (hash-set! t x result)
                result))))

(provide memo-factory)

(define (zip . l)
  (if (ormap empty? l) '()
      (cons (map car l) (apply zip (map cdr l)))))

(check-equal? (zip '(1 2) '(3 4)) '((1 3) (2 4)))

(provide zip)

(define (choose n r)
  (/ (factorial n)
     (* (factorial r) (factorial (- n r)))))

(check-equal? (choose 5 3) 10)
(check-equal? (choose 23 10) 1144066)
(check-equal? (choose 1 1) 1)

(provide choose)