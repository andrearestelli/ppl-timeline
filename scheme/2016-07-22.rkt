#lang racket

; Define the iterate function, with two parameters f and v, that returns
; the infinite list (v f(v) f(f(v)) ... f(f(f(f(f(v))))) ...)

(define (iterate f v)
  (cons v (delay (iterate f (f v)))))

; Define take, like in Haskell, to get items out of an infinite list.
; e.g. (take 10 (iterate (lambda (x) (+ x 1)) 0)) should return (0 1 2 3 4 5 6 7 8 9)

(define (take n iL)
  (cond [(eqv? n 0) '()]
        [else (cons (car iL) (take (- n 1) (force (cdr iL))))]))