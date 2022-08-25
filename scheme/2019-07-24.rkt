#lang racket

; Write a functional, tail recursive implementation of a procedure that takes a list of numbers L and two values
; x and y, and returns three lists: one containing all the elements that are less than both x and y, the second one 
; containing all the elements in the range [x,y], the third one with all the elements bigger than both x and y. It 
; is not possible to use the named let construct in the implementation.

(define (trifilter L x y)
  (define (trifilterH L x y accumL accumM accumR)
    (if (null? L)
        (list accumL accumM accumR)
        (let ((el (car L))
              (els (cdr L)))
          (cond
            [(< el x) (trifilterH els x y (cons el accumL) accumM accumR)]
            [(and (>= el x) (<= el y)) (trifilterH els x y accumL (cons el accumM) accumR)]
            [else (trifilterH els x y accumL accumM (cons el accumR))]))))
  (trifilterH L x y '() '() '()))
            