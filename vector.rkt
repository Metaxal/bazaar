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

;; Renaming the built-in vector-argmin to vector-min,
;; defaulting to returning just the minimum value in the vector
;; proc: any/c -> real
(define (vector-min vec [proc values])
  (vector-argmin proc vec))

;; Returns the index of the first minimum element in the vector according
;; to proc.
;; proc: any/c -> real
;; Returns #f if the vector is empty
;; Should be named vector-argmin, but it is already defined.
(define (vector-min-index vec [proc values])
  (for/fold ([best-index #f]
             [best-value +inf.0]
             #:result best-index)
            ([x (in-vector vec)]
             [i (in-naturals)])
    (define v (proc x))
    (if (< v best-value)
        (values i v)
        (values best-index best-value))))

(module+ test
  (check-equal? (vector-min-index #(1 2 0 5 3 4))
                2)
  (check-equal? (vector-min-index #(1 2 0 5 3 4) -)
                3))
