#lang racket

; 1.1
; in named let construct, the result returned is the value of the last expression, in this case
; when we have finished iterating on the list (x == length(L)) we return the result

(define (co-sublist L i j)
  (let label ((x 0) (result '()))
    (if (< x (length L))
        (if (or (< x i) (> x j))
            (label (+ x 1) (append result (list (list-ref L x))))
            (label (+ x 1) result))
        result)))

; 1.2
; (subl 1 -> 2 3 4 <- 5 6) should be (2 3 4)

; dummy functions to return symbols when encountering -> and <-
; so that later we can use eq? on those symbols to track when to start appending elements to the result
(define -> '->)
(define <- '<-)

(define (subl . args) ; args is the cadr of a list (variable number of arguments) since it is passed as a sequence of numbers, not as a list
   (let loop ((state #f)
             (result '())
             (lst args))
    (cond
      ((null? lst) result)
      ((eq? (car lst) '->)
       (loop #t result (cdr lst)))
      ((eq? (car lst) '<-)
       (loop #f result (cdr lst)))
      (state
       (loop state (append result (list (car lst))) (cdr lst)))
      ((not state)
       (loop state result (cdr lst))))))