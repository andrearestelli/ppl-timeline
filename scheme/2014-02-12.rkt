#lang racket

; Consider a procedure string-from-strings that receives as input a list of objects, keeps all the objects
; that are strings, discarding all the others, and returns the ordered concatenation of all such strings.
; E.g. (string-from-strings ’(1 "hello" ", " 2 "world")) is "hello, world"

; Define a functional (non tail) recursive version of string-from-strings (without using map, filter, fold).

(define (string-from-strings L)
  (if (null? L)
      ""
      (let ((x (car L))
            (xs (cdr L)))
        (if (string? x)
            (string-append x (string-from-strings xs))
            (string-from-strings xs)))))

; Define a tail recursive version of string-from-strings (without using map, filter, fold).

(define (string-from-strings-tail L)
  (define (string-from-strings-h L res)
    (if (null? L)
        res
        (let ((x (car L))
              (xs (cdr L)))
          (if (string? x)
              (string-from-strings-h xs (string-append res x))
              (string-from-strings-h xs res)))))
  (string-from-strings-h L ""))

; Give an implementation of string-from-strings using the classical functional higher order functions, i.e.
; map, filter, fold...

(define (string-from-strings-hof L)
  (foldr string-append "" (filter (λ (x) (string? x)) L)))
                        