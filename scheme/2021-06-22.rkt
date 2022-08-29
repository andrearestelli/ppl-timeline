#lang racket

; Define a function mix which takes a variable number of arguments x0 x1 x2 ... xn, the first one a function, 
; and returns the list (x1 (x2 ... (x0(x1) x0(x2) ... x0(xn)) xn) xn-1) ... x1).

(define (mix f . rest)
  (foldr (λ (x y) (list x y x))
         (map f rest)
         rest))