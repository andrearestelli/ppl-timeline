#lang racket

; Define a pure function f with a variable number of arguments, that, when called like (f x1 x2 .. xn), returns: 
; (xn (xn-1 ( .. (x1 (xn xn-1 .. x1))..). Function f must be defined using only fold operations for loops.

(define (f . args)
  (foldl (λ (x y)
           (list x y))
         (foldl cons '() args)
         args))