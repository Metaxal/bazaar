#lang racket/base

(require bazaar/struct)

(provide (all-defined-out))

(module+ test
  (require rackunit))

;;; Rolling stack (made-up name)
;;; Fixed number N of elements
;;; Keeps the N last pushed elements
;;; O(1) push of the newest element
;;; O(1) pop of the newest element
;;; O(1) access time to any of the last N elements

;; nmax : max number of elements
;; vec : vector containing the elements
;; n : current number of elements
;; idx : current index of the head
(struct roll-stack (nmax vec n idx)
  #:prefab
  #:mutable
  #;#:transparent)

(define (make-roll-stack nmax)
  (roll-stack nmax (make-vector nmax #f) 0 -1))

;; Adds a new element to the head, possibly writing over an old one.
(define (roll-stack-push! rs elt)
  (with-struct
   rs roll-stack (nmax n vec idx)
   (define new-idx (if (= n 0) 0 (modulo (+ idx 1) nmax)))
   (vector-set! vec new-idx elt)
   (set-roll-stack-idx! rs new-idx)
   (when (< n nmax)
     (set-roll-stack-n! rs (+ n 1)))))

(define (roll-stack-pop! rs)
  (with-struct
   rs roll-stack (n nmax idx)
   (when (= 0 n)
     (error "Cannot pop from empty roll-stack"))
   (define elt (roll-stack-ref rs 0))
   (set-roll-stack-idx! rs (modulo (- idx 1) nmax))
   (set-roll-stack-n! rs (- n 1))
   elt))

;; 0 is the head
(define (roll-stack-ref rs pos)
  (with-struct
   rs roll-stack (nmax vec n idx)
   (define ref (modulo (- idx pos) nmax))
   (vector-ref vec ref)))

(define (roll-stack->list rs)
  (for/list ([i (in-range (roll-stack-n rs))])
    (roll-stack-ref rs i)))

(define (roll-stack-fill! rs val)
  (vector-fill! (roll-stack-vec rs) val)
  (set-roll-stack-n! rs (roll-stack-nmax rs)))

(module+ test
  (require (except-in bazaar/debug check-=)
           racket/list)
  (let* ([N 10]
         [rs (make-roll-stack N)]
         [l (build-list 30 (λ(i)(random 100)))]
         [n-elt 0])
    (void
     (for/fold ([lrev '()])
               ([e l])
       (when (> n-elt 0)
         (define idx (random (min n-elt N)))
         #;(debug-vars n-elt idx lrev rs)
         #;(newline)
         (check =
                (roll-stack-ref rs idx)
                (list-ref lrev idx))
         (when (= 0 (random 10)) ; remove first element
           (define elt (first lrev))
           (set! lrev (rest lrev))
           (set! n-elt (sub1 n-elt))
           (define rs-elt (roll-stack-pop! rs))
           (check-equal? elt rs-elt)))
       (check-equal? (roll-stack->list rs)
                     (take lrev (min n-elt N)))
       (roll-stack-push! rs e)
       (set! n-elt (min (add1 n-elt) N))
       (cons e lrev))))
  )
