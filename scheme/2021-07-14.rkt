#lang racket

; Define a defun construct like in Common Lisp, where (defun f (x1 x2 ...) body) is used for defining a 
; function f with parameters x1 x2 .... 
; Every function defined in this way should also be able to return a value x by calling (ret x).

(define-syntax (defun)
  (syntax-rules ()
    ((_ name (arg ...) body ...)
     (define (name arg ...)
       (call/cc
        (lambda (ret)
          body ...))))))