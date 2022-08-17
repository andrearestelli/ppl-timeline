#lang racket

(define (numberlist L state)
  (if (null? L)
      '()
      (let* ((x (car L)) ; let* to use x, just defined, in the following definition of a local variable
             (newstate (+ x state)))
        (cons (cons x newstate) (numberlist (cdr L) newstate)))))