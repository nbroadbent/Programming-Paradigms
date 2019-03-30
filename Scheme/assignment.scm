#lang racket/base
; Question 1.
(define (lover x)
  (map (lambda(n)
         (if (= n 0)
             0
             (/ 1 n))) x))
(lover '(0 2 3 4 12 0 0 1 0))

; Question 2.
(define TOL 1e-6)
(define (newtonRhap x f df)
  (let ((x1 (- x (/ (f x) (df x)))))
    (if (< (abs (- x1 x)) TOL)
        x1
        (newtonRhap x1 f df))))

; Question 3.
(define pi 3.1459)
(define (p_cos_calc x n)
  (let ((top (* 4 (* x x))) (bottom (* (* pi pi) (* (- (* 2 n) 1) (- (* 2 n) 1)))))
    (if (> n 1)
        (let ((x1 (* x (- 1 (/ top bottom)))))
          (if (< (abs (- x1 x)) TOL)
              x1
              (p_cos_calc x1 (+ 1 n))))
        (let ((x1 (- 1 (/ top bottom))))
              (p_cos_calc x1 (+ 1 n))))))
(define (p_cos x)
  (p_cos_calc x 1))

; Question 4
; A
(define (separator? x)
  (if (or (equal? x #\space) (equal? x #\tab) (equal? x #\newline)) 
      #t
      #f))

; B
(define (cpy_h x l n)
  (if (< n (- (length x) 1))
      (let ((value (list-ref x n)))
        (if (separator? value)
            l
            (cpy_h x (append l (cons value '())) (+ 1 n))))
  l))
(define (cpy x)
  (cpy_h x '() 0))
  
  