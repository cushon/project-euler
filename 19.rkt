#lang racket

; Find the number of Sundays in the interval [1 Jan 1901, 31 Dec 2000]

(define (solve)
  
  (define days '(monday tuesday wednesday thurday friday saturday sunday))
  (define months '(january february march april may june july august september october november december))
  
  (define (days-in-month month year)
    (cond [(equal? month 'january) 31]
          [(equal? month 'february) 
           (if (or (zero? (remainder year 400))
                   (and (not (zero? (remainder year 100))) 
                        (zero? (remainder year 4))))
               29
               28)]
          [(equal? month 'march) 31]
          [(equal? month 'april) 30]
          [(equal? month 'may) 31]
          [(equal? month 'june) 30]
          [(equal? month 'july) 31]
          [(equal? month 'august) 31]
          [(equal? month 'september) 30]
          [(equal? month 'october) 31]
          [(equal? month 'november) 30]
          [(equal? month 'december) 31]))
  
  (define (date day-of-week day-of-month month year count)
    (cond
      [(> year 2000) count]
      [(empty? day-of-week) (date days day-of-month month year count)]
      [(empty? month) (date day-of-week day-of-month months (add1 year) count)]
      [(> day-of-month (days-in-month (car month) year))
       (date day-of-week  1 (cdr month) year count)]
      [else (date (cdr day-of-week) (add1 day-of-month) month year
                  (if (and (>= year 1901) 
                           (<= year 2000) 
                           (equal? (car day-of-week) 'sunday)
                           (= day-of-month 1)) 
                      (add1 count) count))]))
  
  (date days 1 months 1900 0))

(provide solve)