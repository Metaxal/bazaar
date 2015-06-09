#lang racket/base

(provide (all-defined-out))

(define (build-points f a b)
    (for/list ([i (in-range a (+ b 1))])
      (list i (f i))))

;; Useful for calls like:
#;(plot (lines (build-points f 15 45)))

