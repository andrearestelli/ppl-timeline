#lang racket

; Define a procedure, called ftree, which takes two nested lists
; (with any possible nesting depth), one containing functions and
; one containing data, and applies the functions to the data provided
; at the same position of the function. When ftree is called with an
; empty first parameter, it works like the identity function.
; e.g.
; (define f1 (lambda (x) (+ 1 x)))
; (define f2 (lambda (x) (* 2 x)))
; (define f3 (lambda (x) (- x 10)))
; (define f4 (lambda (x) (string-append "<<" x ">>")))
; (define t1 '(1 (2 3 4) (5 (6)) ("hi!" 8)))
; (define o1 `(,f1 (,f1 ,f2 ,f1) (,f3 (,f1)) (,f4 ,f3)))
; (define o2 `(,f1 () (,f3 (,f1)) (,f4 ,f3)))
; (ftree o1 t1) must return (2 (3 6 5) (-5 (7)) (<<hi!>> -2))
; (ftree o2 t1) must return (2 (2 3 4) (-5 (7)) (<<hi!>> -2))


(define (ftree fL eL) ; fL is the list of list of functions, eL is the list of data
  (if (or (null? fL) (null? eL))
      '()
      (let ((f (car fL))
            (e (car eL)))
        (cond [(empty? f) (cons e (ftree (cdr fL) (cdr eL)))]
              [(or (list? f)(list? e)) (cons (ftree f e) (ftree (cdr fL) (cdr eL)))]
              [else (cons (f e) (ftree (cdr fL) (cdr eL)))]))))

; more elegant solution

(define (myftree treef tree)
  (cond
    ((null? treef) tree)
    ((list? treef) (cons (myftree (car treef)(car tree))
                         (myftree (cdr treef)(cdr tree))))
    (else ; should be atoms
     (treef tree))))
