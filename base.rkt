#lang racket/base

(provide (all-defined-out))

(define (round-decimal x n)
  (define r (expt 10 n))
  (/ (round (* x r)) r))

(define (bound x low up)
  (min (max x low) up))

;; Returns the value expression and the quoted expression
(define-syntax-rule (expr->expr+symbol expr)
  (values expr 'expr))

;; Returns the quoted variable name and its binding in an assoc
(define-syntax-rule (var->name.var var)
  (cons 'var var))

