#lang racket/base
; Question 1.
(define (lover x)
  (map (lambda(n)
         (if (= n 0)
             0
             (/ 1 n))) x))

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
  (if (< n (length x))
      ; Get value in list at index n, alt use car
      (let ((value (list-ref x n)))
        (if (separator? value)
            l
            (cpy_h x (append l (cons value '())) (+ 1 n))))
  l))
(define (cpy x)
  (cpy_h x '() 0))

; C
(define (drop x)
  (let ((head (car x)) (l (cdr x)))
    (if (separator? head)
        (cpy l)
        (drop l))))

; D
(define (same? l1 l2)
  (if (and (> (length l2) 0) (> (length l1) 0))
      (let ((head1 (car l1)) (head2 (car l2)) (newL1 (cdr l1)) (newL2 (cdr l2)))
        (if (equal? head1 head2)
            (if (separator? head1)
                #t
                (same? newL1 newL2))
            #f))
      #t))
  
; E
(define (checkFirst list key chars)
  ; Check if the first element is a key and the next is a separator
  (let ((head (car list)) (next (cadr list)) (newList (cdr list)))
    (if (and (equal? head (car key)) (separator? next))
        (append chars newList)
        newList)))
(define (checkRest list finalList key chars)
  (if (> (length list) 2)
      ; Take the next three chars and match the pattern (separator key separator)
      (let ((front (car list)) (middle (cadr list)) (end (caddr list)) (newList (cdr list)))
        (if (and (separator? front) (separator? end))
            ; Rebuild the list into finalList based on the matched pattern
            (if (equal? middle (car key))
                (checkRest (cddr newList) (append finalList (cons front (append chars (cons end '())))) key chars)
                (checkRest newList (append finalList (cons front (cons chars '()))) key chars))
            (checkRest newList (append finalList (cons front '())) key chars)))
      ; Check the end of the list
      (let ((front (car list)) (end (cadr list)))
        (if (and (separator? front) (equal? end (car key)))
            (append finalList (cons front (cons chars '())))
            (append finalList (cons front (cons end '())))))))
       
(define (replace list key chars)
  ; First check if the first element should be replace
  (let ((newList (checkFirst list key chars)))
    ; Now check the rest of the list
    (checkRest newList '() key chars)))
  
;#| Testing Solutions
(display "Q1") (newline)
(lover '(0 2 3 4 12 0 0 1 0))
(newline) (newline)

(display "Q2") (newline)
(newtonRhap 0.1 sin cos) 
(newtonRhap 2.0 (lambda (x) (- (* x x) x 6)) (lambda (x) (- (* 2 x) 1))) 
(newtonRhap -20.0 (lambda (x) (- (* x x) x 6)) (lambda (x) (- (* 2 x) 1))) 
(newline) (newline)

(display "Q3") (newline)
(p_cos 0)
(p_cos (/ pi 2)) 
(newline) (newline)

(display "Q4") (newline)
(display "A") (newline)
(separator? #\space)
(separator? #\b)
(display "B") (newline)
(cpy '(#\H #\e #\l #\l #\o #\space #\W #\o #\r #\l #\d))
(display "C") (newline)
(drop '(#\H #\e #\l #\l #\o #\newline #\W #\o #\r #\l #\d))
(display "D") (newline)
(same? '(#\H #\e #\l #\l #\o #\tab #\W #\o #\r #\l #\d) '(#\H #\e #\l #\l #\o))
(same? '(#\H #\e #\l #\l #\o #\space #\W #\o #\r #\l #\d) '(#\W #\o #\r #\l #\d))
(display "E") (newline)
(replace '(#\a #\space #\b #\i #\r #\d #\space #\e #\a #\t #\s #\space #\a #\space #\t #\o #\m #\a #\t #\o) '(#\a) '(#\t #\h #\e)) 
(newline) (newline)
;|#
  