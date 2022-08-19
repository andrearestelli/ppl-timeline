#lang racket

; define a mutable binary tree data structure, using structs.
(struct leaf (
              (value #:mutable)
              ))

(struct branch (
                left
                right))

(define (displayTree t)
  (cond [(leaf? t) (display (leaf-value t))]
        [else (display "[ ")
              (displayTree (branch-left t))
              (display " ")
              (displayTree (branch-right t))
              (display " ]")]))

; Define a destructive map operation for binary trees, called tmap!.
; e.g. (define t1 (branch (branch (leaf 1)(leaf 2))(leaf 3)));
; after (map! (lambda (x) (+ x 1)) t1), t1 becomes 
; (branch (branch (leaf 2)(leaf 3))(leaf 4))

(define (map! f tree)
  (cond [(leaf? tree) (set-leaf-value! tree (f (leaf-value tree)))]
        [else (map! f (branch-left tree))
              (map! f (branch-right tree))]))

; Define a destructive reverse, called reverse!, which takes a binary
; tree and keeps it structure, but “reversing” all the values in 
; the leaves. E.g. (reverse! t1) makes t1 the tree (branch (branch (leaf 3)(leaf 2))(leaf 1))

(define (buildList tree)
  (cond [(leaf? tree) (list (leaf-value tree))]
        [else (append (buildList (branch-left tree))
              (buildList (branch-right tree)))]))

(define (reverse! tree)
  (let ((nums (reverse (buildList tree)))
        (index 0))
    (define (reverseH! tree)
      (cond [(leaf? tree ) (set-leaf-value! tree (list-ref nums index)) (set! index (+ index 1))]
            [else (reverseH! (branch-left tree)) (reverseH! (branch-right tree))]))
    (reverseH! tree)))

