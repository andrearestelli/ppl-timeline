#lang racket

; Give a purely functional definition of fep, which takes a list (x1 x2 ... xn)
; and returns (x1 (x2 (... (xn (x1 x2 ... xn) xn) xn-1) ...) x1).

(define (fep L)
  (foldr (λ (x acc)
           (list x acc x))
         L
         L))

