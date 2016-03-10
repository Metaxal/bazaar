#lang racket/base

(provide (all-defined-out))

(define (round-decimal x n)
  (define r (expt 10 n))
  (/ (round (* x r)) r))


