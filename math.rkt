#lang racket/base

(require define2
         racket/list
         racket/math
         math/base)

(provide (all-defined-out))

(module+ test
  (require rackunit))

;; DEPRECATED: Use the `sgn` function in racket/math or math/flonum
(define (sign x)
  (cond [(< x 0) -1]
        [(> x 0) 1]
        [else 0]))

;; A more numerically stable normalization
;; xs: (listof real?)
(define (flnormalize xs)
  (define s (sum xs))
  (map (λ (x) (/ x s)) xs))

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

;; Willem's 'magic' sequence
;; https://oeis.org/A006519
(define (A6519 j)
  (arithmetic-shift (+ 1 (bitwise-xor j (- j 1))) -1))

(define willems A6519)

(module+ test
  (check-equal? (build-list 32 (compose A6519 add1))
                '(1 2 1 4 1 2 1 8 1 2 1 4 1 2 1 16 1 2 1 4 1 2 1 8 1 2 1 4 1 2 1 32)))

;; Sequence used in Luby's scheduling, 'Reluctant doubling' as Knuth called it
;; https://oeis.org/A182105
(define (A182105 j)
    (if (= (+ j 1) (A6519 (+ j 1)))
        (/ (+ j 1) 2)
        (luby (- (+ j 1) (expt 2 (exact-floor (log j 2)))))))

(define luby A182105)

(module+ test
  (check-equal? (build-list 32 (compose A182105 add1))
                '(1 1 2 1 1 2 4 1 1 2 1 1 2 4 8 1 1 2 1 1 2 4 1 1 2 1 1 2 4 8 16 1)))
