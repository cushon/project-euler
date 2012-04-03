#lang racket
(require  "common.rkt")

; Find the minimal path sum from the top left to bottom right of a matrix,
; usin Dijkstra's algorithm.

(define (solve)
  
  (define matrix
    (map (lambda (x) (map string->number (csv->lst x)))
         (file->lines "83.txt")))
  
  (define width (length (car matrix)))
  (define height (length matrix))
  
  (define arr (list->vector (reverse (foldl append '() (map reverse matrix)))))
  (define dist (make-vector (* width height) +inf.0))
  
  (define (coord-get arr posn)
    (vector-ref arr (+ (first posn) (* (second posn) width))))
  
  (define (coord-set! arr posn v)
    (vector-set! arr (+ (first posn) (* (second posn) width)) v))
  
  (define (get-nbrs x y)
    (map second
         (filter (lambda (x) (first x))
                 (list
                  (list (not (zero? x)) (list (sub1 x) y))
                  (list (not (= x (sub1 width))) (list (add1 x) y))
                  (list (not (zero? y)) (list x (sub1 y)))
                  (list (not (= y (sub1 height))) (list x (add1 y)))))))
  
  (coord-set! dist (list 0 0) 0)
  
  (define points
    (map (lambda (x) (list (remainder x width) (quotient x width)))
         (build-list (* width height) values)))
  
  (define (comparator x y) (<= (coord-get dist x) (coord-get dist y)))
  
  (define (min-first lst comparator)
    (define (h lst least acc)
      (if (empty? lst) (cons least acc)
          (if (comparator (car lst) least)
              (h (cdr lst) (car lst) (cons least acc))
              (h (cdr lst) least (cons (car lst) acc)))))
    (if (or (empty? lst) (empty? (cdr lst)))
        lst
        (h (cdr lst) (car lst) '())))
  
  ; This is stupidly slow because I don't want to implement a fast rekeyable
  ; priority queue, so I do a linear search for the minimal candidate instead
  ; of an O(1) removal from a PQ.
  ;
  ; TODO: fib-heap...
  
  (define (solve Q)
    (if (empty? Q) (void)
        (let ((u (car Q)))
          (for-each
           (lambda (nbr)
             (define alt (+ (coord-get arr nbr)
                            (coord-get dist u)))
             (if (< alt (coord-get dist nbr))
                 (coord-set! dist nbr alt)
                 (void)))
           (apply get-nbrs u))
          (solve (min-first (cdr Q) comparator)))))
  
  (solve points)
  
  (+ (coord-get arr (list 0 0))
     (coord-get dist (list (sub1 width) (sub1 height)))))

(provide solve)