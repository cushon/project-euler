#lang racket
(require "common.rkt" srfi/13)

; Find the 3-character key to the rolling-XOR cipher.

(define (solve)
  
  (define options (range (char->integer #\a) (char->integer #\z)))
  
  (define keys (map flatten (foldl cartesian-product options (list options options))))
  
  (define (string->bits str)
    (map char->integer (string->list str)))
  
  (define (bits->string bits)
    (list->string (map integer->char bits)))
  
  (define (encrypt string key)
    (define (h str k)
      (cond [(empty? str) '()]
            [(empty? k) (h str key)]
            [(cons (bitwise-xor (car k) (car str)) (h (cdr str) (cdr k)))]))
    (h (map char->integer (string->list string)) key))
  
  (define (solve text keys)
    (define (h k)
      (or (empty? k)
          (local ((define plain (bits->string (encrypt text (car k)))))
            (if (and (not (boolean? (string-contains plain "everything")))
                     (> (foldl (lambda (x y) (if (char-alphabetic? x) (add1 y) y))
                               0 (string->list plain))
                        (* (string-length plain) 0.7)))
                (car k)
                (h (cdr k))))))
    (h keys))
  
  (define text (bits->string (map string->number (csv->lst (file->string "59.txt")))))
  
  (define key (solve text keys))
  (foldr + 0 (encrypt text key)))

(provide solve)