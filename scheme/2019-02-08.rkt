#lang racket

; Define a pure function multi-merge with a variable number of arguments (all of them must be ordered lists of
; numbers), that returns an ordered list of all the elements passed. It is forbidden to use external sort functions.
; e.g. when called like:
; (multi-merge '(1 2 3 4 8) '(-1 5 6 7) '(0 3 8) '(9 10 12))
; it returns: '(-1 0 1 2 3 3 4 5 6 7 8 8 9 10 12)

(define (merge ll lr accum)
  (cond [(and (null? ll) (null? lr)) accum]
        [(null? ll) (merge ll (cdr lr) (append accum (list (car lr))))]
        [(null? lr) (merge (cdr ll) lr (append accum (list (car ll))))]
        [(<= (car ll) (car lr)) (merge (cdr ll) lr (append accum (list (car ll))))]
        [(> (car ll) (car lr)) (merge ll (cdr lr) (append accum (list (car lr))))]))

(define (multi-merge . args)
  (foldl (λ (l acc)
           (merge l acc '()))
         '()
         args))
  