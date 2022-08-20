#lang racket

(define (p L)
  (define (pHelper L stateS stateD accumS accumD accum)
    (if (null? L)
        accum
        (let ((x (car L)) (xs (cdr L)))
          (cond  [(eqv? x '*)
                  (if stateS
                      (pHelper xs #f stateD 0 accumD (append accum (list accumS)))
                      (pHelper xs #t stateD 0 accumD accum))]
                 [(eqv? x '$)
                  (if stateD
                      (pHelper xs stateS #f accumS "" (append accum (list accumD)))
                      (pHelper xs stateS #t accumS "" accum))]
                 [(number? x)
                  (if stateS
                      (pHelper xs stateS stateD (+ accumS x) accumD accum) ; asterisc is opened -> sum x to the accumulator
                      (pHelper xs stateS stateD accumS accumD accum))]     ; asterisc is not opened -> discard x
                 [(string? x)
                  (if stateD
                      (pHelper xs stateS stateD accumS (string-append accumD x) accum) ; dollar is opened -> append x to the accumulator
                      (pHelper xs stateS stateD accumS accumD accum))]                 ; dollar has not been opened -> discard x
                 [else (pHelper xs stateS stateD accumS accumD accum)]))))
  (pHelper L #f #f 0 "" '()))
