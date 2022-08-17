#lang racket

(define (producer ag1 ag2)
  (let loop ((i 1))
    (if (< i 10)
        (begin
          ((if (odd? i) ag1 ag2) i)
          (loop (+ i 1)))
        (cons
         (ag2 'end)
         (ag1 'end)))))

; clos1 should return a lambda that collects the parameter passed into a list 
(define (clos1)
  (let ((state '()))
    (λ (i)
      (if (eq? i 'end)
          state
          (set! state (cons i state))
          ))))
      

; clos2 should return a lambda that sums the parameter passed to its internal state
(define (clos2)
  (let ((state 0))
    (λ (i)
      (if (eq? i 'end)
          state
          (set! state (+ state i))
          ))))