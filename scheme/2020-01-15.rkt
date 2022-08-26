#lang racket

; Consider the Foldable and Applicative type classes in Haskell. We want to implement something 
; analogous in Scheme for vectors. Note: you can use the following library functions in your code: vectormap, vector-append.
; 1) Define vector-foldl and vector-foldr.

(define (vector-foldl f z vec)
  (let loop ((res z)
             (i 0)
             (len (vector-length vec)))
    (cond
      [(< i len) (loop (f (vector-ref vec i) res) (+ i 1) len)]
      [else res])))

(define (vector-foldr f z vec)
  (let loop ((res z)
             (i (- (vector-length vec) 1)))
    (cond
      [(>= i 0) (loop (f (vector-ref vec i) res) (- i 1))]
      [else res])))

; 2) Define vector-pure and vector-<*>.

(define (vector-pure x)
  (vector x))

; to define <*>, we have to define concat and concatmap

(define (vector-concat vov)
  (vector-foldr (λ (x y) (vector-append x y)) #() vov))

(define (vector-concatmap f vec)
  (vector-concat (vector-map f vec)))

(define (vector-<*> fs xs)
  (vector-concatmap (λ (f) (vector-map f xs)) fs))