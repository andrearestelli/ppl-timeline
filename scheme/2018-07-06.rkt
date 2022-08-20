#lang racket

; Give a purely functional definition of deepen,
; which takes a list (x1 x2 ... xn) and returns ((... ((x1) x2) ...) xn).

(define (deepen L)
  (define (deepenH  accum lst)
    (cond [(null? lst) accum]
          [(deepenH (list accum (car lst)) (cdr lst))]))
  (if (null? L)
    '()
    (deepenH (list (car L)) (cdr L))))

; more elegant

(define (mydeepen L)
  (foldl (λ (x y)       ; y is the accumulator
           (list y x))  ; x is the current element
         (list (car L))
         (cdr L)))

; Write the construct define-with-return:, which takes a name m, used as a return function,
; a list function name + parameters, and a function body, and defines a function with the
; same characteristics, where calls to m are used to return a value.
; e.g. if we define
; (define-with-return: return (f x) ; note that the function name is f, while return is used, of course, for returning
;  (define a 12)
;  (return (+ x a))
;  'unreachable), 
; a call (f 3) should give 15.

(define-syntax define-with-return:
  (syntax-rules ()
    ((_ name func body ...)
     (define func
       (call/cc (λ (name)
                  body ...))))))

(define-with-return: return (f x) 
    (define a 12)
    (return (+ x a))
    'unreachable)
