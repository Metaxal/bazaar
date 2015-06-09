#lang racket/base

(provide define/memoize
         memoize)

(define (memoize f)
  (define h (make-hash))
  (λ args
    (hash-ref! h args (λ()(apply f args)))))

;; Simple memoization
(define-syntax-rule (define/memoize (f args ...) body ...)
  (begin
    ; One hash per function
    (define h (make-hash))
    (define (f args ...)
      (hash-ref! h (list args ...) (λ()body ...)))))

(module+ test
  (require rackunit)
  
  (define foo
    (memoize (λ(x y)(+ (random) x y))))
  
  (define/memoize (bar x y)
    (+ (random) x y))
  
  (check = (foo 2 3) (foo 2 3))
  (check = (bar 4 5) (bar 4 5)))

