#lang racket

; Find the number of (alphabetic) characters required to write out the numbers from
; 1 to 1000.

(define (solve)
  
  (define start '(one two three four five six seven eight nine ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen))
  (define tens '(() twenty thirty forty fifty sixty seventy eighty ninety))
  (define hundred 'hundred)
  
  (define (list-ref n lst)
    (if (= 1 n) (first lst)
        (list-ref (sub1 n) (cdr lst))))
  
  (define (pretty-print n)
    (cond [(<= n 19) (format "~a " (list-ref n start))]
          [(<= n 99) (string-append (format "~a " (list-ref (quotient n 10) tens))
                                    (if (not (zero? (remainder n 10))) 
                                        (pretty-print (remainder n 10)) ""))]
          [(<= n 999) (string-append (format "~a hundred " 
                                             (list-ref (quotient n 100) start))
                                     (if (not (zero? (remainder n 100)))
                                         (string-append "and " 
                                                        (pretty-print (remainder n 100)))
                                         ""))]
          [(= n 1000) "one thousand"]))
  
  (define (count-letters s)
    (length (filter (lambda (x) (not (char-whitespace? x))) (string->list s))))
  
  (foldl + 0 (map count-letters (map pretty-print (build-list 1000 add1)))))

(provide solve)