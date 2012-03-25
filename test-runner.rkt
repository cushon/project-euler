#lang racket
(require rackunit)

(define expected
  '((1 233168)
    (2 4613732)
    (3 6857)
    (4 906609)
    (5 232792560)
    (6 25164150)
    (7 104743)
    (8 40824)
    (9 31875000)
    (10 142913828922)
    (11 70600674)
    (12 76576500)
    (13 5537376230)
    (14 837799)
    (15 137846528820)
    (16 1366)
    (17 21124)
    (18 1074)
    (19 171)
    (20 648)))

(define (test-problem p s)
  (printf "Test ~a\t" p)
  (define result ((dynamic-require (format "~a.rkt" p) 'solve)))
  (if (equal? result s)
      (printf "...OK\n")
      (printf "...FAILED (Got ~a, Expected ~a\n)" result s)))

(define (test n)
  (match (assoc (and n (string->number n)) expected)
    [(list problem-number solution) (test-problem problem-number solution)]
    [else
     (for-each
      (lambda (x) (apply test-problem x))
      expected)]))

(define args (current-command-line-arguments))
(test (and (not (zero? (vector-length args)))
           (vector-ref (current-command-line-arguments) 0)))