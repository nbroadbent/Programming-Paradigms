#lang racket/base
(define Q '(1 2 (a 3 4 5)))
(caaddr Q)   
;;;;;;;;;;;;;;;;;;;;;
; (caaddr Q)    
; => 'a
;;;;;;;;;;;;;;;;;;;