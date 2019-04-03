#lang racket
(current-directory)

(define (readTableau fileIn)  
  (let ((sL (map (lambda s (string-split (car s))) (file->lines fileIn))))
    (map (lambda (L)
           (map (lambda (s)
                  (if (eqv? (string->number s) #f)
                      s
                      (string->number s))) L)) sL)))

(define (parseHeader x)
  (car x))
(define (parseData x n data)
  (if (> (length x) 0)
      (if (= n 0)
          (parseData (cdr x) 1 data)
          (parseData (cdr x) 2  (cons (cdr (car x)) data)))
      (reverse data)))
  

(define (parseSupply x supply)
  (if (> (length x) 1)
      (let ((len (- (length (car x)) 1)) (row (car x)))
        (parseSupply (cdr x) (append supply (cons (list-ref row len) '()))))
      supply))

(define (parseDemand x)
  (list-ref x (- (length x) 1)))

(define (parseCost x cost)
  (if (> (length x) 1)
      (parseCost (cdr x) (cons (reverse (cdr (reverse (car x)))) cost))
      (reverse cost)))

(define (makeList n l)
  (if (> n 0)
      (makeList (- n 1) (cons 0 l))
      l))

(define (makeMatrix n m matrix)
  (if (> n 0)
      (makeMatrix (- n 1) m (cons (makeList m '()) matrix))
      matrix))
    
(define tb (readTableau "3x3.txt"))
(define header (parseHeader tb))
(define data (parseData tb 0 '()))
(define supply (parseSupply data '()))
(define demand (parseDemand data))
(define cost (parseCost data '()))
(define supplied (makeMatrix (length cost) (length (car cost)) '()))

(define (replace1d l i n value newList)
  (if (> (length l) 0)
      (if (= n i)
          (replace1d (cdr l) i (+ n 1) value (cons value newList))
          (replace1d (cdr l) i (+ n 1) value (cons (car l) newList)))
      (reverse newList)))

(define (replace2d l i j n value newMatrix)
  (if (> (length l) 0)
      (if (= n i)
          (replace2d (cdr l) i j (+ n 1) value (cons (replace1d (car l) j 0 value '()) newMatrix))
          (replace2d (cdr l) i j (+ n 1) value (cons (car l) newMatrix)))
      (reverse newMatrix)))

(define (index matrix i j)
  (list-ref (list-ref matrix i) j))

(define (initMinRow i j)
  (if (< j (length (list-ref supplied i)))
      ; Check for demand and not supplied.
      (if (and (> (list-ref demand j) 0) (= (index supplied i j) 0))
            (index cost i j)
            (initMinRow i (+ j 1)))
      #f))

; Function to initilize min to find a valid lowest cost.
(define (initMin i)
  (if (< i (length supply))
      ; Check if row has supply.
      (if (> (list-ref supply i) 0)
          (let ((x (initMinRow i 0)))
            ;(display x) (display " space ")
            (if (number? x)
                x
                (initMin (+ i 1))))
          (initMin (+ i 1)))
      #f))

(define (minCostRow min i j minj)
  (if (< j (length (list-ref supplied i)))
      ; Check for node with demand and not yet supplied and is minimum.
      (if (and (> (list-ref demand j) 0) (= (index supplied i j) 0) (<= (index cost i j) min))
          (minCostRow (index cost i j) i (+ j 1) j)
          (minCostRow min i (+ j 1) minj))
      (cons (index cost i minj) (cons i minj))))

; Function to initilize min to find a valid lowest cost.
(define (minCost i min mini minj)
  ; Check if row has supply.
  (if (< i (length supply))
      (if (> (list-ref supply i) 0)
          (let ((minRow (minCostRow (initMin i) i 0 0)) (i1 (+ i 1)))
            (if (<= (car minRow) min)
                (minCost i1 (car minRow) i (cdr (cdr minRow)))
                (minCost i1 min mini minj)))
          (minCost (+ i 1) min mini minj))
       (cons (index cost mini minj) (cons mini minj))))

;(set! demand (replace1d demand 0 0 0 '()))
;demand
;(initMin 0)
;(minCostRow (initMin 0) 0 0 0)
;(minCost 0 (initMin 0) 0 0)


(define (shipItems to)
  ; Get amount to transfer to location "to"
  (let ((amount (min (list-ref supply (car to)) (list-ref demand (cdr to)))))
    (set! supply (replace1d supply (car to) 0 (- (list-ref supply (car to)) amount) '()))
    (set! demand (replace1d demand (cdr to) 0 (- (list-ref demand (cdr to)) amount) '()))
    (set! supplied (replace2d supplied (car to) (cdr to) 0 amount '()))))

(define (done? l)
  (if (> (length l) 0)
      (if (not (= (car l) 0))
          #f
          (done? (cdr l)))
      #t))

(define (greedySolution)
  (if (or (done? supply) (done? demand))
      #t
      (begin
        (display (or (done? supply) (done? demand))) (newline)
        (display "init ") (display (initMin 0)) (newline)
        (display "demand ") (display demand) (newline)
        (display "supply ") (display supply) (newline)
        (display "min :") (display (minCost 0 (initMin 0) 0 0)) (newline)
        (let ((min (initMin 0)))
          (if (number? min)
              (begin
                (display min)
                (shipItems (cdr (minCost 0 min 0 0)))
                (display "After demand ") (display demand) (newline)
                (display "After supply ") (display supply) (newline)
                (newline) (newline)
                (greedySolution))
              #t)))))
  
(define (writeTableau tb fileOut)
  (if (eqv? tb '())
      #t
      (begin (display-lines-to-file (car tb) fileOut #:separator #\space #:exists 'append)
             (display-to-file #\newline fileOut #:exists 'append)
             (writeTableau (cdr tb) fileOut))))

(define (labelSolution i newList)
  (if (< i (length supplied))
      (labelSolution (+ i 1) (cons (append (list-ref supplied i) (list-ref supplyCopy i)) newList))
      (reverse newList)))
  
(define supplyCopy (apply list supply))
(define demandCopy (apply list demand))
  
cost
(greedySolution)
(define s (labelSolution 0 '()))
(writeTableau s "solution.txt")
