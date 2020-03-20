#lang racket/base
(require racket/vector)

(provide (all-defined-out)
         (rename-out [vector-ref vref]
                     [vector-set! vset!]
                     [vector-update! vupdate!]
                     [vector-length vlength]))

(module+ test
  (require rackunit))

(define (vector-update! vec i updater)
  (vector-set! vec i (updater (vector-ref vec i))))

(module+ test
  (let ([vec (make-vector 3 1)])
    (vector-update! vec 1 add1)
    (check = (vector-ref vec 1) 2)))

;; Wrapping procedure to avoid writing vector-ref, vector-set! and vector-update! all the time
;; The two-argument case is an updater if proc-or-value is a procedure of arity 1,
;; or a setter otherwise
(define (vector->proc vec)
  (case-lambda
    [()    vec]
    [(idx) (vector-ref vec idx)]
    [(idx proc-or-value)
     (if (and (procedure? proc-or-value)
              (procedure-arity-includes? proc-or-value 1))
         (vector-update! vec idx proc-or-value)
         (vector-set! vec idx proc-or-value))]))

(module+ test
  (let ([v (vector->proc (make-vector 3 1))])
    (check = (v 2) 1)
    (v 2 5)
    (check = (v 2) 5)
    (check = (v 1) 1)
    (v 2 add1)
    (check-equal? (v) #(1 1 6))))

;; Returns an element of v chosen uniformly
(define (vector-choose v)
  (vector-ref v (random (length v))))

(define (vector-normalize! v)
  (define sum (for/sum ([x (in-vector v)]) x))
  (vector-map! (λ(x)(/ x sum)) v))

;; Returns the best value (key x) of the elements x of the vector vec,
;; along with its corresponding index in the vector.
;; Values are compared with `better?`---if `(better? a b)` is not `#f` this
;; means that `a` is better than `b`.
;; The vector vec is assumed non-empty.
;; vec : non-empty-vector?
;; better? : T T -> boolean?
;; key? : any/c -> T
(define (vector-best+index vec better? #:key [key values])
  (for/fold ([best-value (key (vector-ref vec 0))]
             [best-index 0])
            ([x (in-vector vec 1)]
             [i (in-naturals 1)])
    (define v (key x))
    (if (better? v best-value)
        (values v i)
        (values best-value best-index))))

(module+ test
  (check-equal? (call-with-values (λ () (vector-best+index #(1 2 0 5 3 4) <))
                                  list)
                '(0 2))
  (check-equal? (call-with-values (λ () (vector-best+index #(1 2 0 5 3 4) >))
                                  list)
                '(5 3))
  (check-equal? (call-with-values (λ () (vector-best+index #(1 2 0 5 3 4) < #:key -))
                                  list)
                '(-5 3)))

;; Like `vector-best+index` but returns only the best element (not its index)
(define (vector-best vec better? #:key [key values])
  (define-values (best index)
    (vector-best+index vec better? #:key key))
  best)

;; Like `vector-best+index` but returns only the index (not the element)
(define (vector-best-index vec better? #:key [key values])
  (define-values (best index)
    (vector-best+index vec better? #:key key))
  index)

(module+ test
  (check-equal? (vector-best-index #(1 2 0 5 3 4) <)
                2)
  (check-equal? (vector-best-index #(1 2 0 5 3 4) < #:key -)
                3))



