#lang racket

; Define a <*> operator for lists, defined as in Haskell’s Applicative Functors.
; e.g. (<*> (list (lambda (x) (+ 1 x))
;                 (lambda (x) (* 2 x))) 
;           '(1 2 3))
; is the list ‘(2 3 4 2 4 6).

(define (my<*> lF list)
  (cond [(null? lF) '()]
        [else (append (map (car lF) list) (<*> (cdr lF) list))]))

; more elegant solution

(define (concatmap f ls)
  (foldr append '() (map f ls)))

(define (<*> fs xs)
  (concatmap (lambda (f) (map f xs)) fs))