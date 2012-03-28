#lang racket
(require rackunit)

; Find the number of hands player 1 wins in the collection of poker hands.

(define (solve)
  
  (define (split n lst)
    (define (h n lst acc)
      (if (zero? n) (list (reverse acc) lst)
          (h (sub1 n) (cdr lst) (cons (car lst) acc))))
    (h n lst '()))
  
  (define (tokenize str)
    (define (h lst acc)
      (cond [(empty? lst) (list (reverse acc))]
            [(char-whitespace? (car lst))
             (if (empty? acc) (h (cdr lst) acc)
                 (cons (reverse acc) (h (cdr lst) '())))]
            [else (h (cdr lst) (cons (car lst) acc))]))
    (h (string->list str) '()))
  
  (define hands
    (map (lambda (x) (split 5 x))
         (map tokenize
              (file->lines "54.txt"))))
  
  (define (flush? hand)
    (= (length (remove-duplicates (map second hand))) 1))
  
  (define suit (string->list "123456789TJQKA"))
  
  (define (index-of q lst)
    (define (h n lst)
      (if (equal? q (car lst)) n
          (h (add1 n) (cdr lst))))
    (h 0 lst))
  
  (define (straight? hand)
    (define (consecutive? lst)
      (define (h last lst)
        (cond [(empty? lst) #t]
              [else (and (= (add1 last) (car lst)) (h (car lst) (cdr lst)))]))
      (or (empty? lst) (h (car lst) (cdr lst))))
    (consecutive? (sort (map (lambda (x) (index-of x suit)) (map first hand)) <)))
  
  (check-equal? (straight? '((#\8 #\C) (#\T #\S) (#\K #\C) (#\9 #\H) (#\4 #\S))) #f)
  (check-equal? (straight? '((#\Q #\C) (#\T #\S) (#\K #\C) (#\J #\H) (#\A #\S))) #t)
  
  (define (group lst)
    (define (h lst last count)
      (cond
        [(empty? lst) (list (list last count))]
        [(equal? (car lst) last) (h (cdr lst) last (add1 count))]
        [else (cons (list last count) (h (cdr lst) (car lst) 1))]))
    (define sorted (sort lst char<?))
    (sort (h (cdr sorted) (car sorted) 1) (lambda (x y) 
                                            (if (= (second x) (second y))
                                                (> (index-of (first x) suit)
                                                   (index-of (first y) suit))
                                                (> (second x) (second y))))))
  
  (define (score hand)
    (cond
      [(and (equal? (sort (map first hand) char<?)
                    (sort '(#\K #\T #\J #\Q #\A) char<?))
            (flush? hand))
       'royal-flush]
      [(and (straight? hand) (flush? hand)) 'straigh-flush]
      [(= (second (first (group (map first hand)))) 4) 'four-of-a-kind]
      [(and 
        (= (second (first (group (map first hand)))) 3)
        (= (second (second (group (map first hand)))) 2)) 'full-house]
      [(flush? hand) 'flush]
      [(straight? hand) 'straight]
      [(= (second (first (group (map first hand)))) 3) 'three-of-a-kind]
      [(and 
        (= (second (first (group (map first hand)))) 2)
        (= (second (second (group (map first hand)))) 2)) 'two-pair]
      [(= (second (first (group (map first hand)))) 2) 'pair]
      [else 'high]))
  
  (check-equal? (score '((#\T #\S) (#\Q #\S) (#\K #\S) (#\A #\S) (#\J #\S)))
                'royal-flush)
  (check-equal? (score '((#\Q #\S) (#\A #\S) (#\K #\S) (#\T #\S) (#\J #\S)))
                'royal-flush)
  (check-equal? (score '((#\T #\H) (#\Q #\H) (#\K #\H) (#\A #\H) (#\J #\H)))
                'royal-flush)
  (check-equal? (score '((#\T #\S) (#\T #\H) (#\T #\H) (#\T #\H) (#\J #\H)))
                'four-of-a-kind)
  (check-equal? (score '((#\T #\S) (#\T #\H) (#\T #\H) (#\A #\H) (#\A #\H)))
                'full-house)
  (check-equal? (score '((#\1 #\H) (#\T #\H) (#\T #\H) (#\A #\H) (#\J #\H)))
                'flush)
  (check-equal? (score '((#\1 #\H) (#\2 #\H) (#\3 #\H) (#\4 #\H) (#\5 #\S)))
                'straight)
  (check-equal? (score '((#\T #\S) (#\T #\H) (#\T #\H) (#\A #\H) (#\J #\H)))
                'three-of-a-kind)
  (check-equal? (score '((#\T #\S) (#\T #\H) (#\A #\H) (#\A #\H) (#\J #\H)))
                'two-pair)
  (check-equal? (score '((#\T #\S) (#\T #\H) (#\1 #\H) (#\A #\H) (#\J #\H)))
                'pair)
  (check-equal? (score '((#\T #\S) (#\2 #\H) (#\1 #\H) (#\A #\H) (#\J #\H)))
                'high)
  
  (define scores '(royal-flush straigh-flush four-of-a-kind full-house flush straight three-of-a-kind two-pair pair high))
  
  (define (full-score hand)
    (define (tie-break-values hand)
      (map (lambda (x) (index-of x suit)) (map first (group (map first hand)))))
    (cons (index-of (score hand) (reverse scores))
          (tie-break-values hand)))
  
  (define (list>? a b)
    (cond [(or (empty? a) (empty? b)) #f]
          [(> (car a) (car b)) #t]
          [(< (car a) (car b)) #f]
          [else (list>? (cdr a) (cdr b))]))
  
  (check-equal? (list>? '(3 4 1) '(3 4 0)) #t)
  (check-equal? (list>? '(3 4 1) '(3 4 1)) #f)
  (check-equal? (list>? '(3 4 1) '(3 5 0)) #f)
  (check-equal? (list>? '(3 4 1) '(2 5 0)) #t)
  (check-equal? (list>? '(3 4 1 1) '(3 4 1 1)) #f)
  (check-equal? (list>? '(3 4 1 1) '(3 4 1 2)) #f)
  (check-equal? (list>? '(3 4 1 2) '(3 4 1 1)) #t)
  (check-equal? (list>? '(4 4 1 1) '(3 4 1 1)) #t)
  (check-equal? (list>? '(4 4 1 1) '(5 4 1 1)) #f)
  (check-equal? (list>? '(3 4 1) '(3 4 1 0)) #f)
  
  (check-equal?
   (length
    (map
     (lambda (x) (list>? (full-score (first x)) (full-score (second x))))
     hands))
   1000)
  
  (length
   (filter values
           (map
            (lambda (x) (list>? (full-score (first x)) (full-score (second x))))
            hands))))

(provide solve)
  