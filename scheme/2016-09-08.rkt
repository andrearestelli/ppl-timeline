#lang racket

; In the academic literature, there is a concept of pictures defined as
; rectangular arrays of symbols, e.g.
;              abb
;              bab
; Such pictures can of course be memorized by row, as lists of list, e.g.
; the previous picture is ‘((a b b)(b a b)).
; Consider the language L of pictures where symbols are from the set {0,1},
; and are square pictures with 1 on the diagonal and 0 elsewhere
; (e.g. ‘((1 0 0)(0 1 0)(0 0 1))).
; Define a procedure, called genFig, which takes a natural number n and returns
; the picture of L with side n.

(define (buildRow dim pos)
  (let loop ((result '())
             (x 0))
    (if (< x dim)
        (if (= x pos)
            (loop (append result (list 1)) (+ x 1))
            (loop (append result (list 0)) (+ x 1)))
        result)))
       
(define (genFig n)
  (define (genFigH dim curr)
    (cond [(= curr n) '()]
          [else (cons (buildRow dim curr) (genFigH dim (+ curr 1)))]))
  (genFigH n 0))