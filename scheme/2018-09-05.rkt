#lang racket

; Define in a purely functional way a procedure called revlt, which takes three lists, (x1 ... xL) (y1 ... yM) (z1 .. zN)
; and returns the list of vectors: (#(xO yO zO) ... #(x1 y1 z1)), where O ≥ 1 is the smallest among L, M, and N.
; e.g. (revlt '(1 2 3) '(4 5 6 7) '(8 9 10)) is the list '(#(3 6 10) #(2 5 9) #(1 4 8)).

(define (revlt lfst lsnd ltrd)
  (let loop ((i 0)
             (top (min (length lfst) (length lsnd) (length ltrd)))
             (result '()))
    (if (< i top)
        (loop (+ i 1) top (cons (vector (list-ref lfst i) (list-ref lsnd i) (list-ref ltrd i)) result))
        result)))