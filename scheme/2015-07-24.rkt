#lang racket

; 1.1
; define a tail recursive procedure, called urmax, that takes a list of lists and returns
; the max among the n-th elements of each n-th list
; e.g. (urmax ’((-1)(1 2)(1 2 3)(10 2 3 -4))) is (max -1 2 3 -4) , thus 3.

(define (urmax L)
  (define (urmax-tail L accum pos) ; local procedure tail recursive needed to add an accumulator
    (if (null? L)
        (apply max accum) ; because accum is a list and we want to apply max to the sequence of numbers in the list
        (urmax-tail (cdr L) (cons (list-ref (car L) pos) accum) (+ pos 1))))
  (urmax-tail L '() 0))

; 1.2
; define a variant of urmax based on higher order functions like map (you cannot use iterative loops
; or recursion in it)

(define (urmax-map L)
  (apply max (map (lambda (l) (list-ref l (index-of L l))) L))
 )