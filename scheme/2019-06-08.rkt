#lang racket

; Consider this data definition in Haskell: data Tree a = Leaf a | Branch (Tree a) a (Tree a)
; Define an OO analogous of this data structure in Scheme using the technique of "closure as classes" as seen 
; in class, defining the map and print methods, so that: 
; (define t1 (Branch (Branch (Leaf 1) -1 (Leaf 2)) -2 (Leaf 3)))
; ((t1 'map (lambda (x) (+ x 1))) 'print)
; should display: (Branch (Branch (Leaf 2) 0 (Leaf 3)) -1 (Leaf 4))

(define (Leaf v)
  (let ((value v))
    ; methods
    (define (print)
      (display "Leaf ")
      (display value))
    (define (map f)
      (Leaf (f v)))
    (λ (message . args)
      (apply (case message
               ((print) print)
               ((map) map)
               (else (error "Unknown message")))
             args))))

(define (Branch l v r)
  (let ((left l)
        (value v)
        (right r))
    ; methods
    (define (print)
      (display "Branch (")
      (l 'print)
      (display ") ")
      (display value)
      (display " (")
      (r 'print)
      (display ")"))
    (define (map f)
      (Branch (l 'map f) (f value) (r 'map f)))
    (λ (message . args)
      (apply (case message
               ((print) print)
               ((map) map)
               (else (error "Unknown message")))
             args))))