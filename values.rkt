#lang racket/base

(require define2
         (for-syntax racket/base)
         racket/match)

(provide (all-defined-out))

(module+ test
  (require rackunit
           racket/list))

;; Return the multiple values of proc-call as a list
(define-syntax-rule (values->list expr)
  (call-with-values (λ()expr) list))
; Example:
; (values->list (values 1 2 3))
; -> '(1 2 3)

; Idea by Jens Axel Søgaard
(define-syntax defv (make-rename-transformer #'define-values))
(define-syntax letv (make-rename-transformer #'let-values))
(define-syntax defm (make-rename-transformer #'match-define))

(define-syntax-rule (first-value expr)
  (call-with-values (λ()expr) (λ(a . r) a)))

(define-syntax-rule (second-value expr)
  (call-with-values (λ()expr) (λ(_a b . r) b)))

(define-syntax-rule (third-value expr)
  (call-with-values (λ()expr) (λ(_a _b c . r) c)))

(define-syntax-rule (fourth-value expr)
  (call-with-values (λ()expr) (λ(_a _b _c d . r) d)))

(define-syntax-rule (fifth-value expr)
  (call-with-values (λ()expr) (λ(_a _b _c _d e . r) e)))

(module+ test
  (let ([l '(10 20 30 40 50 60)])
    (check-equal? (list (first-value (apply values l))
                        (second-value (apply values l))
                        (third-value (apply values l))
                        (fourth-value (apply values l))
                        (fifth-value (apply values l))
                        (sixth l))
                  l)))

