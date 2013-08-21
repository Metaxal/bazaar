#lang racket/base

(provide (all-defined-out))

(define-syntax-rule (while test body ...)
  (let loop ()
    (when test
      body ...
      (loop))))

(define-syntax-rule (until test body ...)
  (let loop ()
    (unless test
      body ...
      (loop))))
