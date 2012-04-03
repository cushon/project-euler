#lang racket
(require (prefix-in srfi: srfi/48) "common.rkt")

; Find the fixed point probability of occupying a given square in a game
; of monopoly, assuming that dice have four sides.

; Represent the probability of occupying each of 40 squares as a system
; of linear equations, and solve the corresponding matrix. The model is
; incomplete / incorrect (it doesn't handle the probability of rolling
; doubles and going to jail), but is a sufficiently good approximation
; to produce the right ordering of the top three states...

; The matrix solver was only tested on this matrix, and is massively
; inefficient.

(define (solve)
  
  (define tiles '(GO A1 CC1 A2 T1 R1 B1 CH1 B2 B3
                     JAIL C1 U1 C2 C3 R2 D1 CC2 D2 D3
                     FP E1 CH2 E2 E3 R3 F1 F2 U2 F3
                     G2J G1 G2 CC3 G3 R4 CH3 H1 T2 H2))
  
  (define tile-map (make-hash))
  
  (for-each
   (lambda (x) (apply (lambda (x y) (hash-set! tile-map x y)) x))
   (zip tiles (range 0 (length tiles))))
  
  (define (tile->int n) (hash-ref tile-map n))
  
  (define tile-vector (list->vector tiles))
  
  (define (int->tile n) (vector-ref tile-vector n))
  
  (define size (length tiles))
  
  (define dist '(0 0 1 2 3 4 3 2 1))
  
  (define (roll-prob roll)
    (define distribution (list->vector dist))
    (if (>= roll (vector-length distribution)) 0
        (/ (vector-ref distribution roll)
           (foldr + 0 dist))))
  
  (define (positive n) (if (<= n 0) 0 n))
  
  (define prob-triple (/ 1 36))
  
  (define (get-rp a b) (roll-prob (modulo (- b a) size)))
  
  (define (CC-prob a b)
    (define rp (roll-prob (modulo (- b a) size)))
    (match (vector-ref tile-vector b)
      ['GO (+ (/ 1 16) (* (/ 14 16) rp))]
      ['JAIL (+ (/ 1 16) (* (/ 14 16) rp))]
      [else (* (/ 14 16) rp)]))
  
  (define (CH-prob a b next-R next-U back-3)
    (define rp (roll-prob (modulo (- b a) size)))
    (match (vector-ref tile-vector b)
      ['GO (+ (/ 1 16) (* (/ 6 16) rp))]
      ['JAIL (+ (/ 1 16) (* (/ 6 16) rp))]
      ['C1 (+ (/ 1 16) (* (/ 6 16) rp))]
      ['E3 (+ (/ 1 16) (* (/ 6 16) rp))]
      ['H2 (+ (/ 1 16) (* (/ 6 16) rp))]
      ['R1 ((lambda (x) (if (equal? next-R 'R1) (+ x (/ 2 16)) x))
            (+ (/ 1 16) (* (/ 6 16) rp)))]
      [else
       (cond [(equal? (vector-ref tile-vector b) next-R)
              (+ (/ 2 16) (* (/ 6 16) rp))]
             [(equal? (vector-ref tile-vector b) next-U)
              (+ (/ 1 16) (* (/ 6 16) rp))]
             [(equal? (vector-ref tile-vector b) back-3)
              (+ (/ 1 16) (* (/ 6 16) rp))]
             [else (* (/ 6 16) rp)])]))
  
  (define (travel-prob a b)
    (define rp (roll-prob (modulo (- b a) size)))
    (match (vector-ref tile-vector a)
      ['CC1 (CC-prob a b)]
      ['CC2 (CC-prob a b)]
      ['CC3 (CC-prob a b)]
      ['CH1 (CH-prob a b 'R2 'U1 'T1)]
      ['CH2 (CH-prob a b 'R3 'U2 'D3)]
      ['CH3 (CH-prob a b 'R1 'U1 'CC3)]
      ['G2J (if (equal? (vector-ref tile-vector b) 'JAIL) 1 0)]
      [else rp]))
  
  (define mtx
    (map (lambda (y)
           (map (lambda (x) (if (= y x) 1 (- (travel-prob x y))))
                (build-list size values)))
         (build-list size values)))
  
  (define (normalize v i)
    (define start (vector-ref v i))
    (if (zero? start) #f
        (vector-map! (lambda (x) (/ x start)) v)))
  
  (define (elim a b i)
    (define factor (/ (vector-ref b i) (vector-ref a i)))
    (for-each (lambda (x) 
                (vector-set! b x 
                             (- (vector-ref b x) (* factor (vector-ref a x)))))
              (range i (sub1 (vector-length b)))))
  
  (define (solve mtx i)
    (if (> i (min (sub1 (vector-length (vector-ref mtx 1)))
                  (sub1 (vector-length mtx))))  
        
        (for-each
         (lambda (x)
           ((lambda (a b i)
              (if (zero? (vector-ref a i)) (void)
                  (vector-set! b x (/ (vector-ref b i) (vector-ref a i)))))
            (vector-ref mtx (sub1 (vector-length mtx))) (vector-ref mtx x) 0))
         (range 0 (- (vector-length mtx) 3)))
        
        (begin
          
          (and (normalize (vector-ref mtx i) i)
               (for-each
                (lambda (x)
                  (or (= x i)
                      (elim (vector-ref mtx i) (vector-ref mtx x) i)))
                (range 0 (sub1 (vector-length mtx)))))
          
          (solve mtx (add1 i)))))
  
  (define (solve-wrapper lst-mtx)
    (define mtx (list->vector (map list->vector lst-mtx)))
    (solve mtx 0)
    (vector->list 
     (vector-map (lambda (x) (vector-ref x (sub1 (vector-length x)))) mtx)))
  
  (define soln (solve-wrapper mtx))
  
  (define (glue lst) (list->int (flatten (map int->list lst))))
  
  (glue
   (map (lambda (x) (tile->int (second x)))
        (take
         (sort
          (zip (map (lambda (x) (abs (/ x (foldr + 0 soln)))) soln)
               (map (lambda (x) (vector-ref tile-vector x))
                    (build-list size values)))
          (lambda (x y) (> (first x) (first y)))) 3))))

(provide solve)