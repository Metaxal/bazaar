#lang racket/base

(require plot
         racket/dict)

(provide (all-defined-out)
         (all-from-out plot))

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

(define (dict-histogram d)
  (discrete-histogram
   (for/list ([(k v) (in-dict d)])
     (vector k v))))

#; ; Ex:
(plot
   (dict-histogram
   '((a . 10) (b . 20) (c . 12))))

(define-syntax-rule (with-x-log-transform body ...)
  (parameterize ([plot-x-transform log-transform]
                 [plot-x-ticks (log-ticks)])
    body ...))

(define-syntax-rule (with-y-log-transform body ...)
  (parameterize ([plot-y-transform log-transform]
                 [plot-y-ticks (log-ticks)])
    body ...))

(define-syntax-rule (with-xy-log-transform body ...)
  (parameterize ([plot-x-transform log-transform]
                 [plot-y-transform log-transform]
                 [plot-x-ticks (log-ticks)]
                 [plot-y-ticks (log-ticks)])
    body ...))

