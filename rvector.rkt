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
  
  (define N 40000000)

  (displayln "vector (best size in 'hindsight'):")
  (time
   (let ([v (make-vector N)]) ; `let' ensures v is garbage-collected once out of scope
     (for ([i (in-range N)])
       (vector-set! v i i))))

  (collect-garbage)
  (collect-garbage)
  (displayln "gvector:")
  (time
   (let ([gv (make-gvector)])
     (for ([i (in-range N)])
       (gvector-set! gv i i))))

  (collect-garbage)
  (collect-garbage)
  (displayln "rvector:")
  (time
   (let ([rv (make-rvector)])
     (for ([i (in-range N)])
       (rvector-set! rv i i))))

  (collect-garbage)
  (collect-garbage)
  (displayln "rvector (best in hindsight):")
  (time
   (let ([rv (make-rvector N)])
     (for ([i (in-range N)])
       (rvector-set! rv i i))))

#|
Results:

vector (best size in 'hindsight'):
cpu time: 285 real time: 285 gc time: 5
gvector:
cpu time: 9987 real time: 9981 gc time: 5010
rvector:
cpu time: 1608 real time: 1608 gc time: 332
rvector (best in hindsight):
cpu time: 869 real time: 868 gc time: 1

Comments:
- On this test, rvectors are more than 5 times faster than gvectors (most of it being gc).
- a growing rvector really is within a time factor 2 of rvector initialized with the correct size.
- Compared to `vector', there are a few overheads.
  The check in rvector-ref is actually costly.
|#
  )
