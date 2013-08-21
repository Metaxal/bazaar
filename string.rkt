#lang racket/base

(provide (all-defined-out))

(define (string?-ci=? a b)
  (and (string? a) (string? b)
       (string-ci=? a b)))

(define (string-reverse str)
  (list->string (reverse (string->list str))))

