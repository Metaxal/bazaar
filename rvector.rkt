#lang racket/base
(require racket/vector)

(provide #;(struct-out rvector)
         (rename-out [construct-rvector rvector])
         make-rvector
         build-rvector
         rvector-copy
         rvector-length
         rvector-ref
         rvector-set!
         rvector=?
         rvector->vector
         vector->rvector
         rvector->list)

;;; Resizable vectors.
;;; Much faster than data/gvector.

;;; When inserting a new element at a place that is outside the current range,
;;; the vector is resized to the smallest power of 2.
;;; Without garbage collection,
;;; this ensures (mostly) both a computation time within a factor 2 (*)
;;; and a memory cost within a factor 4 of the best vector size in hindsight.
;;; The garbage collector will however (tend to) retrade some computation time
;;; for a factor 2 on the memory cost.
;;; (*) in terms of read/write of elements--does not include memory allocation time.

;;; When the vector is resized, it is filled with the default value given in make-rvector.
;;; An element can be set at any (positive) position in the vector, the vector will be
;;; automatically extended to include this position.

;;; The main problem is that arguments should be checked, which would incur a
;;; substantial overhead. Checks are still performed at a deeper level but errors
;;; may not be very intelligible.

;;; TODO:
;;; - gen:custom-write
;;; - gen:set (?)
;;; - gen:dict (?)

(define (rvector-hash-1 rvec recursive-equal-hash)
  (define n (rvector-length rvec))
  (+ n
     (if (> n 0)
         (* 10 (recursive-equal-hash (rvector-ref rvec 0)))
         0)
     (if (> n 1)
         (* 100 (recursive-equal-hash (rvector-ref rvec (- n 1))))
         0)))

(define (rvector-hash-2 rvec recursive-equal-hash)
  (define n (rvector-length rvec))
  (+ (* 100 n)
     (if (> n 0)
         (* 1 (recursive-equal-hash (rvector-ref rvec 0)))
         0)
     (if (> n 1)
         (* 10 (recursive-equal-hash (rvector-ref rvec 1)))
         0)))

(define (rvector=? rvec1 rvec2 recursive-equal?)
  (define n (rvector-length rvec1))
  (and (= n (rvector-length rvec2))
       (let ([vec1 (rvector-vec rvec1)]
             [vec2 (rvector-vec rvec2)])
         (for/and ([i (in-range n)])
           (recursive-equal? (vector-ref vec1 i)
                             (vector-ref vec2 i))))))

(struct rvector (vec length default-value)
  #:mutable
  #:transparent
  #:constructor-name make-struct-rvector
  #:methods gen:equal+hash
  [(define equal-proc rvector=?)
   (define hash-proc rvector-hash-1)
   (define hash2-proc rvector-hash-2)]
  )

(define (make-rvector [n 1] [default-value 0])
  (make-struct-rvector (make-vector (expt 2 (integer-length (- n 1)))
                                    default-value)
                       n
                       default-value))

(define (construct-rvector #:default-value [default-value 0]
                           . lv)
  (define n (length lv))
  (define rvec (make-rvector n default-value))
  (define vec (rvector-vec rvec))
  (for ([x (in-list lv)]
        [i (in-naturals)])
    (vector-set! vec i x))
  rvec)

(define (build-rvector n proc [default-value 0])
  (define rvec (make-rvector n default-value))
  (define vec (rvector-vec rvec))
  (for ([i (in-range n)])
    (vector-set! vec i (proc i)))
  rvec)

;; Same as vector-ref.
(define (rvector-ref rvec i)
  ; This check slows things down quite substantially! (compared to raw `vector')
  (if (< i (rvector-length rvec))
      (vector-ref (rvector-vec rvec) i)
      (error "Index out of bounds" i 'length: (rvector-length rvec))))

;; Inserts a new element v at position i in the rvector.
;; If the index is out of the current bounds, the vector is first resized.
(define (rvector-set! rvec i v)
  (define vec (rvector-vec rvec))
  (if (< i (vector-length vec))
      (begin
        (vector-set! vec i v)
        (when (>= i (rvector-length rvec))
          (set-rvector-length! rvec (+ i 1))))
      (begin
        (rvector-resize! rvec (+ i 1))
        (vector-set! (rvector-vec rvec) i v))))

;; Resize the vector to the closest power of 2 of new-length.
;; Cells between the last length and the new index are filled with the default value
;; given during construction.
(define (rvector-resize! rvec new-length)
  (define new-size (expt 2 (integer-length (- new-length 1))))
  (define vec (rvector-vec rvec))
  (define new-vec (make-vector new-size (rvector-default-value rvec)))
  (vector-copy! new-vec 0 vec)
  (set-rvector-length! rvec new-length)
  (set-rvector-vec! rvec new-vec))

(define (rvector->vector rvec)
  (define n (rvector-length rvec))
  (define vec (make-vector n))
  (vector-copy! vec 0 (rvector-vec rvec) 0 n)
  vec)

(define (vector->rvector vec [default-value 0])
  (define n (vector-length vec))
  (define rvec (make-rvector n default-value))
  (vector-copy! (rvector-vec rvec) 0 vec)
  rvec)

(define (rvector-copy rvec)
  (struct-copy rvector rvec))

(define (rvector->list rvec)
  (define vec (rvector-vec rvec))
  (let loop ([i (- (rvector-length rvec) 1)] [l '()])
    (if (= i -1)
        l
        (loop (- i 1) (cons (vector-ref vec i) l)))))

(define (rvector-fill! rvec v)
  (define vec (rvector-vec rvec))
  (for ([i (in-range (rvector-length rvec))])
    (vector-set! vec i v)))

(module+ test
  (require rackunit
           racket/list)
  
  (define rvec (make-rvector))
  (for ([i 100])
    (rvector-set! rvec i i))
  (check = (rvector-length rvec) 100)
  (check = (rvector-ref rvec 0) 0)
  (check = (rvector-ref rvec 51) 51)
  (check = (rvector-ref rvec 99) 99)
  (check-exn exn:fail? (λ()(rvector-ref rvec 100)))

  (define rvec1 (rvector-copy rvec))
  (check-equal? rvec rvec1)

  (define vec (rvector->vector rvec))
  (check =
         (vector-length vec)
         (rvector-length rvec))
  (for ([i (in-range (vector-length vec))])
    (check =
           (vector-ref vec i)
           (rvector-ref rvec i)))

  (define rvec2 (vector->rvector vec 2))
  (check-equal? rvec rvec2)

  (define rvec3 (build-rvector 100 values 3))
  (check-equal? rvec rvec3)

  (define rvec4 (apply construct-rvector (build-list 100 values)
                       #:default-value 4))
  (check-equal? rvec rvec4)

  (define rvec5 (apply construct-rvector (rvector->list rvec)))
  (check-equal? rvec rvec5)

  (define rv-list (list rvec rvec1 rvec2 rvec3 rvec4))
  (for ([rv (in-list rv-list)]
        [default-value (in-list '(0 0 2 3 4))])
    (rvector-set! rv 200 200)
    (check = (rvector-ref rv 200) 200)
    (check = (rvector-ref rv 199) default-value))
  
  (for* ([rv1 (in-list (cdr rv-list))]
         [rv2 (in-list (cdr rv-list))])
    (unless (eq? rv1 rv2)
      (check-not-equal? rv1 rv2)))

  ;; Check that the vectors are different entities after rvector-copy.
  (rvector-set! rvec1 200 201)
  (check-not-equal? rvec rvec1)

  (rvector-fill! rvec1 10)
  (check-equal? (rvector->list rvec1)
                (make-list 201 10))
  (rvector-set! rvec1 210 210)
  ;; Filling should not change the default value.
  (check = (rvector-ref rvec1 201) 0)
  (check = (rvector-ref rvec1 209) 0)
  
  )

(module+ main

  ;; Simple benchmark.
  ;; Try this on the command line (NOT in DrRacket) after compiling it:
  ;; raco make rvector.rkt
  ;; racket rvector.rkt
  ;;
  ;; This test consumes a lot of memory, so make sure memory greedy applications
  ;; are closed before running this benchmark (e.g., DrRacket).
  (require data/gvector)
  
  (define N 30000000)
  
  (define-syntax-rule (vtest label vmake vset! vref)
    (begin
      (newline)
      (collect-garbage)
      (collect-garbage)
      (displayln label)
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
         make-rvector
         rvector-set!
         rvector-ref)

  (vtest "gvector:"
         make-gvector
         gvector-set! ; works only when increasing the size by at most 1
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
