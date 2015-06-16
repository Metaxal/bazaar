#lang racket/base

(provide (all-defined-out))

(module+ test
  (require rackunit))

(define (build-points f a b [inc 1])
    (for/list ([i (in-range a (+ b 1) inc)])
      (list i (f i))))

;; Useful for calls like:
#;(plot (lines (build-points f 15 45)))

(module+ test
  (check-equal?
   (build-points (λ(x)(* x x)) 1 10 2)
   '((1 1) (3 9) (5 25) (7 49) (9 81))))

(define (list->points l [start 0])
  (for/list ([x l] [i (in-naturals start)])
    (list i x)))

;; Useful for calls like:
#;(plot (lines (list->points '(0.3 0.2 0.75))))

(module+ test
  (check-equal?
   (list->points '(a b c) 2)
   '((2 a) (3 b) (4 c))))