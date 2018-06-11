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
