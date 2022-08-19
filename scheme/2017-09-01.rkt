#lang racket

; Consider a list L of symbols. We want to check if in L there are matching
; “a” and “b” symbols or “1” and “2” symbols, where “a” and “1” have an open
; parenthesis role, while “b” and “2” stand for close parenthesis respectively
; (i.e. a Dyck language); other symbols are ignored. Define a pure and tail
; recursive function check-this which returns the number of matching pairs,
; and #f if the parenthesis structure is not respected. 
; e.g.
; (check-this '(a b a b)) is 2
; (check-this '(h e l l o)) is 0
; (check-this '(6 h a b a 1 h h i 2 b z)) is 3
; (check-this '(6 h a b a 1 h h i b z 2)) is #f (wrong structure)

(define (check-this L)
  (define (checkH L accum stack) ; type is the type of parenthesis that we expect to find if we find a closed one (#f for ab, #t for 12)
    (if (null? L)
        (if (null? stack)
            accum
            #f)
        (case (car L)
          ((1) (checkH (cdr L) accum (cons 1 stack)))
          ((a) (checkH (cdr L) accum (cons 'a stack)))
          ((2) (if (eqv? (car stack) 1)
                   (checkH (cdr L) (+ accum 1) (cdr stack))
                   #f))
          ((b) (if (eqv? (car stack) 'a)
                   (checkH (cdr L) (+ accum 1) (cdr stack))
                   #f))
          (else (checkH (cdr L) accum stack)))))
  (checkH L 0 '()))