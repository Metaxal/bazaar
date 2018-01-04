#lang racket/base
;;; Copyright (C) Laurent Orseau, 2010-2013
;;; GNU Lesser General Public Licence (http://www.gnu.org/licenses/lgpl.html)

(require racket/list
         racket/math)

(provide (all-defined-out))

;; DEPRECATED: Use the `sgn` function in racket/math or math/flonum
(define (sign x)
  (cond [(< x 0) -1]
        [(> x 0) 1]
        [else 0]))

;; points : (listof (list/c real real)) ; list of (x y) coordinates
;; Returns a, b and f(x)=ax+b.
(define (linear-regression points)
  (define n (length points))
  (define xs (map first points))
  (define ys (map second points))
  (define sum-x (apply + xs))
  (define sum-y (apply + ys))
  (define sum-x2 (apply + (map sqr xs)))
  (define sum-xy (apply + (map * xs ys)))
  (define a (/ (- sum-xy (* (/ n) sum-x sum-y))
               (- sum-x2 (* (/ n) (sqr sum-x)))))
  (define b (/ (- sum-y (* a sum-x)) n))
  (values a b (λ(x)(+ (* a x) b))))

(module+ drracket
  (define a1 (* 5 (- (random) .5)))
  (define b1 (* 10 (- (random) .5)))
  (define l1 (build-list 100 (λ(i)(define x (random 100))
                               (list x
                                     (+ (* 2 (random)) ; add some noise
                                        (+ b1 (* a1 x)))))))
  (define-values (a2 b2 f)
    (linear-regression l1))
  (displayln (list 'a1: a1 'a2: a2 'b1: b1 'b2: b2))
  (require plot)
  (plot
   (list (points l1)
         (function f)))
  )