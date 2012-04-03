#lang racket
(require "common.rkt")

; Find the sum of the top left-most 3 cells in the solution to all of the
; sudoku puzzles.

(define (solve)
  
  (define (tok lst fn)
    (define (helper lst acc)
      (cond [(empty? lst)
             (if (cons? acc) (list (reverse acc)) '())]
            [(fn (car lst))
             (if (empty? acc)
                 (helper (cdr lst) acc)
                 (cons (reverse acc) (helper (cdr lst) '())))]
            [else (helper (cdr lst) (cons (car lst) acc))]))
    (helper lst '()))
  
  (define puzzles
    (map
     (lambda (x)
       (map (lambda (x)
              (map (lambda (x) (- (char->integer x) (char->integer #\0)))
                   (string->list x)))
            x))
     (tok (file->lines "96.txt")
          (lambda (x) (string=? (substring x 0 4) "Grid")))))
  
  (define (board-ref b x)
    (vector-ref (vector-ref b (second x)) (first x)))
  
  (define (board-set! b x v)
    (vector-set! (vector-ref b (second x)) (first x) v))
  
  (define (init-game puzzle)
    (for-each
     (lambda (x)
       (if (zero? (board-ref puzzle x))
           (board-set! puzzle x (range 1 9))
           (board-set! puzzle x (board-ref puzzle x))))
     (cartesian-product (range 0 8) (range 0 8))))
  
  (define row-groups
    (map
     (lambda (x) (map (lambda (y) (list y x)) (range 0 8)))
     (range 0 8)))
  
  (define col-groups
    (map (lambda (x) (map reverse x)) row-groups))
  
  (define sector-groups
    (map (lambda (x)
           (cartesian-product (range (* (first x) 3) (+ (* (first x) 3) 2))
                              (range (* (second x) 3) (+ (* (second x) 3) 2))))
         (cartesian-product (range 0 2) (range 0 2))))
  
  (define (set-minus set remove)
    (cond [(or (empty? set) (empty? remove)) set]
          [(< (car remove) (car set))
           (set-minus set (cdr remove))]
          [(> (car remove) (car set))
           (cons (car set) (set-minus (cdr set) remove))]
          [else (set-minus (cdr set) (cdr remove))]))
  
  (define (reduce-group board group)
    (define (h board group last)
      (define group-vals (map (lambda (x) (board-ref board x)) group))
      (or (equal? last group-vals)
          (let ((remove (sort (filter number? group-vals) <)))
            (for-each
             (lambda (x)
               (define v (board-ref board x))
               (board-set! board x (if (cons? v) (set-minus v remove) v)))
             group)
            (for-each
             (lambda (x)
               (define v (board-ref board x))
               (board-set! board x (if (and (cons? v) (empty? (cdr v))) (car v) v)))
             group)
            (h board group group-vals))))
    (h board group #f))
  
  (define (solved? board)
    (define (complete-group? group)
      (foldl (lambda (y x) (and y x)) #t
             (map (lambda (x) (= (length (remove-duplicates (map (lambda (cell) (board-ref board cell)) x))) 9))
                  group)))
    (and
     (foldl (lambda (x y) (and x y)) #t (map (lambda (x) (number? (board-ref board x))) (cartesian-product (range 0 8) (range 0 8))))
     (complete-group? col-groups)
     (complete-group? row-groups)
     (complete-group? sector-groups)))
  
  (define (all-reductions board)
    (define (copy-board board)
      (map vector->list (vector->list board)))
    (define (h board last)
      (define curr-copy (copy-board board))
      (or (equal? curr-copy last)
          (begin
            (for-each (lambda (x) (reduce-group board x)) sector-groups)
            (for-each (lambda (x) (reduce-group board x)) row-groups)
            (for-each (lambda (x) (reduce-group board x)) col-groups)
            (h board curr-copy))))
    (h board #f)
    (solved? board))
  
  (define (solve board)
    (define (copy-board board)
      (list->vector (map list->vector (map vector->list (vector->list board)))))
    (define (h board cells)
      (cond [(empty? cells) #f]
            [(number? (board-ref board (car cells)))
             (h board (cdr cells))]
            [else
             (foldl
              (lambda (x y)
                (or y
                    (begin
                      (board-set! board (car cells) x)
                      (solve (copy-board board)))))
              #f
              (board-ref board (car cells)))]))
    (define r (all-reductions board))
    (if r board
        (h board (cartesian-product (range 0 8) (range 0 8)))))
  
  (define all-puzzles
    (map (lambda (x) (list->vector (map list->vector x)))
         puzzles))
  
  (for-each init-game all-puzzles)
  
  (define (get-proof board)
    (list->int (map (lambda (x) (board-ref board x)) '((0 0) (1 0) (2 0)))))
  
  (foldl + 0 (map get-proof (map solve all-puzzles))))

(provide solve)