#lang racket

; Consider a list L and a natural number k. Define an iterator with a closure,
; which returns, in turn, all the contiguous sublists of L of length k, and a
; symbol 'end at the end. It is possible to use the function take provided by
; Racket, e.g. (take '(a b c) 2) is '(a b).

(define (iter-window L k)
  (let ((list L))
    (lambda ()
      (if (< (length list) k)
          'end
          (let ((result (take list k)))
            (set! list (cdr list))
            result)))))

; Define a function checklist analogous to the one of Exercise 1.3, by using only
; a foldl as the main loop.
; For instance a call (checklist '(b b a a b b b c) '((a b)(b a)(b b))) should return ((b c) (a a)).

(define (checklist list factors)
  (let*  ((k (length (car factors)))
          (iter (iter-window list k)))
    (foldl
     (λ (x r)
       (let ((curr (iter)))
         (if (member curr (cons 'end factors)) ; if the current sublist is in factors, don't add it to the result
             r
             (cons curr r))))                  ; otherwise yes
     '()
     list)))