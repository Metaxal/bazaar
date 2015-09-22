#lang racket/base

(require racket/match)

(provide (all-defined-out))

(module+ test
  (require rackunit))

;;; Rolling stack (made-up name)
;;; Fixed number N of elements
;;; Keeps the N last pushed elements
;;; O(1) push of the newest element
;;; O(1) access time to any element

;; nmax : max number of elements
;; vec : vector containing the elements
;; n : current number of elements
;; idx : current index of the head
(struct roll-stack (nmax vec n idx)
  #:mutable
  #:transparent)

(define (make-roll-stack nmax [default #f])
  (roll-stack nmax (make-vector nmax default) nmax -1))

;; Adds a new element to the head, possibly writing over an old one.
(define (roll-stack-push! rs elt)
  (match-define (roll-stack nmax vec n idx) rs)
  (define new-idx (if (= n 0) 0 (modulo (+ idx 1) n)))
  (vector-set! vec new-idx elt)
  (set-roll-stack-idx! rs new-idx)
  (when (< n nmax)
    (set-roll-stack-n! rs (+ n 1))))

;; 0 is the head
(define (roll-stack-ref rs pos)
  (match-define (roll-stack nmax vec n idx) rs)
  (define ref (modulo (- idx pos) n))
  (vector-ref vec ref))

(module+ test
  (let* ([N 10]
         [rs (make-roll-stack N)]
         [l (build-list 30 (λ(i)(random 100)))])
    (void
     (for/fold ([lrev '()])
               ([e l]
                [i (in-naturals)])
       (when (> i 0)
         (define idx (random (min i N)))
         #;(debug-vars i idx lrev rs)
         (check =
                (roll-stack-ref rs idx)
                (list-ref lrev idx)))
       (roll-stack-push! rs e)
       (cons e lrev)))
    ))
