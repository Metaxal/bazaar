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
