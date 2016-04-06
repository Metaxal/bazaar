#lang racket/base

(provide (all-defined-out))

(module+ test
  (require rackunit))

(define (vector-update! vec i updater)
  (vector-set! vec i (updater (vector-ref vec i))))

(module+ test
  (let ([vec (make-vector 3 1)])
    (vector-update! vec 1 add1)
    (check = (vector-ref vec 1) 2)))

;; Some shortcuts, because long names are sometimes pretty cumbersome,
;; in particular for vectors

;; Wrapping procedure to avoid writing vector-ref, vector-set! and vector-update! all the time
;; The two-argument case is an updater if proc-or-value is a procedure of arity 1,
;; or a setter otherwise
(define (vector->proc vec)
  (case-lambda
    [()    vec]
    [(idx) (vector-ref vec idx)]
    [(idx proc-or-value)
     (if (and (procedure? proc-or-value)
              (procedure-arity-includes? proc-or-value 1))
         (vector-update! vec idx proc-or-value)
         (vector-set! vec idx proc-or-value))]))

