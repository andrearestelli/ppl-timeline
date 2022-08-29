#lang racket

(define (make-man)
  (let ((p (λ (x) (x)))
        (name "man"))
    (define prefix+name
      (lambda (prefix)
        (string-append prefix name)))
    (define change-name
      (lambda (new-name)
        (set! name new-name)))
    (define-dispatcher methods: (prefix+name change-name) parent: p)))

(define-syntax define-dispatcher
  (syntax-rules (methods : parent)
    ((_ methods: (meth ...) parent: par)
     (lambda (message . args)
       (apply (case message
                ((meth) meth)
                ...
                (else (error "Sending message to par")))
              args)))))
      