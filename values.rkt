#lang racket/base

(provide (all-defined-out))

;; Return the multiple values of proc-call as a list
(define-syntax-rule (values->list expr)
  (call-with-values (λ()expr) (λ l l)))
; Example:
; (values->list (values 1 2 3))
; -> '(1 2 3)
