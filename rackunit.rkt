#lang racket/base
(require rackunit)
(provide (all-from-out rackunit)
         check-fail
         check-not-fail)

; These should take an optional argument
(define-syntax-rule (check-fail expr)
  (check-exn exn:fail? (λ()expr)))
(define-syntax-rule (check-not-fail expr)
  (check-not-exn (λ()expr)))
  
(module+ test
  (check-fail (error "coucou"))
  (check-not-fail "hello")
  )
  
