#lang racket/base
(require racket/vector)

(provide (struct-out rvector)
         make-rvector
         rvector-ref
         rvector-set!)

;;; Resizable vectors.
;;; Much faster than data/gvector.

;;; When inserting a new element at a place that is outside the current range,
;;; the vector is resized to the smallest power of 2.
;;; Without garbage collection,
;;; this ensures (mostly) both a computation time within a factor 2 (*)
;;; and a memory cost within a factor 4 of the best vector size in hindsight.
;;; The garbage collector will however (tend to) retrade to factor 4 on the computation time 
;;; and factor 2 on the memory cost.
;;; (*) in terms of read/write of elements--does not include memory allocation time.

(struct rvector (vec length)
  #:mutable
  #:transparent)

(define (make-rvector [n 1] [v 0])
  (rvector (make-vector (expt 2 (integer-length (- n 1))) v)
         n))

;; Returns the value 
(define (rvector-ref rsv i)
  ; This check slows things down quite substantially! (compared to raw `vector')
  (if (< i (rvector-length rsv))
      (vector-ref (rvector-vec rsv) i)
      (error "Index out of bounds" i 'length: (rvector-length rsv))))

;; Inserts a new element v at position i in the rvector.
;; If the index is out of the current bounds, the vector is first resized.
(define (rvector-set! rsv i v)
  (define vec (rvector-vec rsv))
  (if (< i (vector-length vec))
      (begin
        (vector-set! vec i v)
        (when (>= i (rvector-length rsv))
          (set-rvector-length! rsv (+ i 1))))
      (begin
        (rvector-resize! rsv (+ i 1))
        (vector-set! (rvector-vec rsv) i v))))

;; Resize the vector to the closest power of 2 of new-length.
(define (rvector-resize! rsv new-length)
  (define new-size (expt 2 (integer-length (- new-length 1))))
  (define vec (rvector-vec rsv))
  (define new-vec (make-vector new-size))
  (vector-copy! new-vec 0 vec)
  (set-rvector-length! rsv new-length)
  (set-rvector-vec! rsv new-vec))

(module+ test
  (require rackunit)
  (define rsv (make-rvector))
  (for ([i 100])
    (rvector-set! rsv i i))
  (check = (rvector-length rsv) 100)
  (check = (rvector-ref rsv 0) 0)
  (check = (rvector-ref rsv 51) 51)
  (check = (rvector-ref rsv 99) 99)
  (check-exn exn:fail? (λ()(rvector-ref rsv 100)))
  )

(module+ main

  ;; Simple benchmark.
  ;; Try this on the command line (NOT in DrRacket) after compiling it:
  ;; raco make rvector.rkt
  ;; racket rvector.rkt
  (require data/gvector)
  
  (define N 30000000)
  
  (define-syntax-rule (vtest label vmake vset! vref)
    (begin
      (newline)
      (displayln label)
      (collect-garbage)
      (collect-garbage)
      (let ([v (vmake)]) ; `let' ensures v is garbage-collected once out of scope
        (displayln "set!")
        (time
         (for ([i (in-range N)])
           (vset! v i i)))
        (displayln "ref")
        (displayln
         (time
          (for/sum ([i (in-range N)])
            (vref v i)))))))

  
  (vtest "vector (best size in 'hindsight'):"
         (λ()(make-vector N))
         vector-set!
         vector-ref)
  
  (vtest "rvector (best size in 'hindsight'):"
         (λ()(make-rvector N))
         rvector-set!
         rvector-ref)
  
  (vtest "rvector:"
         (λ()(make-rvector))
         rvector-set!
         rvector-ref)

  (vtest "gvector:"
         (λ()(make-gvector))
         gvector-set!
         gvector-ref)

 )
#|
Results:

vector (best size in 'hindsight'):
set!
cpu time: 300 real time: 300 gc time: 125
ref
cpu time: 168 real time: 167 gc time: 0
449999985000000

rvector (best size in 'hindsight'):
set!
cpu time: 518 real time: 517 gc time: 0
ref
cpu time: 484 real time: 483 gc time: 0
449999985000000

rvector:
set!
cpu time: 1077 real time: 1076 gc time: 229
ref
cpu time: 492 real time: 491 gc time: 0
449999985000000

gvector:
set!
cpu time: 6980 real time: 6974 gc time: 3149
ref
cpu time: 2260 real time: 2258 gc time: 200
449999985000000

Comments:
- On this test, rvectors are more than 6-7 times faster than gvectors (half of it being gc).
- a growing rvector really is within a time factor 2 of rvector initialized with the correct size.
- Even when given the correct initial size, the few checks performed by rvectors incur
  a non-negligeable overhead.
- rvector spent 21% of the time of gc, compared to 45% for gvectors.
|#
