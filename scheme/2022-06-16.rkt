#lang racket

; Define a list-to-compose pure function, which takes a list containing functions of one argument and 
; returns their composition. 
; E.g. (list-to-compose (list f g h)) is the function f(g(h(x)).

(define (list-to-compose L)
  (if(null? L)
     (λ (x) x)
     (λ (x)
       ((car L) ((list-to-compose (cdr L)) x)))))

