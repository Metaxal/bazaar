#lang racket/base

(provide (all-defined-out))

(require define2)

(module+ test
  (require rackunit))

(define (round-decimal x n)
  (define r (expt 10 n))
  (/ (round (* x r)) r))

(define (bound x low up)
  (min (max x low) up))

;; Returns the value expression and the quoted expression
(define-syntax-rule (expr->expr+symbol expr)
  (values expr 'expr))

;; Returns the quoted variable name and its binding in an assoc
(define-syntax-rule (vars->name.var var ...)
  (list (cons 'var var) ...))

(module+ test
  (let ([x 3] [y 2])
    (check-equal? (vars->name.var x y)
                  '((x . 3) (y . 2)))))

;; Like in-range, but offsetted by 1
(define (in-range1 nmax)
  (in-range 1 (+ nmax 1)))

