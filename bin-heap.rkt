#lang racket
(require define2
         bazaar/debug)

(provide make-heap heap-empty? heap-insert! heap-remove-min! heap-min)

;; Binary heap
;; https://www.cs.cmu.edu/~adamchik/15-121/lectures/Binary%20Heaps/heaps.html
;; https://en.wikipedia.org/wiki/Binary_heap

;; Also see data/heap:
;; /usr/share/racket-7.0.0.1/pkgs/data-lib/data/heap.rkt

;; TODO: Cache the extracted keys.

;; Index 0 is skipped for convenience.

(define (grow-vector v1)
  (let ([v2 (make-vector (* (vector-length v1) 2) #f)])
    (vector-copy! v2 0 v1)
    v2))

(define (shrink-vector v1)
  (let ([v2 (make-vector (quotient (vector-length v1) 2) #f)])
    (vector-copy! v2 0 v1 0 (vector-length v2))
    v2))


(define MIN-SIZE 4)
(struct heap (hvec N <?)
  #:transparent #:mutable)

;; Returns a heap with elements of type T
;; <? : T×T -> Bool.
;; key: same purpose as in `sort'.
;; seq : sequence (list, vector, etc.) of elements of type T
(define (make-heap <? [seq #f] #:key [extract-key #f])
  (define aheap (heap (make-vector MIN-SIZE #f)
                      0
                      (if extract-key
                          (λ(a b)(<? (extract-key a) (extract-key b)))
                          <?)))
  (when seq
    (for ([x seq])
      (heap-insert! aheap x)))
  aheap)

(define (heap-insert! aheap v)
  (match-define (heap hvec N <?) aheap)
  (set! N (+ N 1))
  (set-heap-N! aheap N)
  (when (= N (vector-length hvec))
    ; Double the size of the vector
    (set! hvec (grow-vector hvec))
    (set-heap-hvec! aheap hvec))
  ; Percolate toward the root with the last element of the heap.
  (define new-pos
    (let loop ([pos N]) ; element starts at last position
      (cond
        [(= pos 1) pos]
        [else
         (define pos2 (quotient pos 2))
         (define v2 (vector-ref hvec pos2))
         (cond [(<? v v2)
                ;; Replace the current position with the parent
                (vector-set! hvec pos v2)
                (loop pos2)]
               [else pos])])))
  ;; We found the position to place the new element
  (vector-set! hvec new-pos v))

(define (heap-min aheap)
  (match-define (heap hvec N <?) aheap)
  (assert (> N 0))
  (vector-ref hvec 1))

(define (heap-empty? aheap)
  (= (heap-N aheap) 0))

;; Removes the minimum element. If v is not #f, it is inserted in the heap at the same
;; time, which avoids one percolation round, and sometimes resizing.
;; Returns the value of the removed element (the min element).
(define (heap-remove-min! aheap [v #f])
  (match-define (heap hvec N <?) aheap)
  (assert (> N 0) hvec)
  (define v-root (vector-ref hvec 1))
  (unless v
    (set! v (vector-ref hvec N))
    (vector-set! hvec N #f)
    (set! N (- N 1))
    (set-heap-N! aheap N)
    (when (< MIN-SIZE N (quotient (vector-length hvec) 4))
      (set! hvec (shrink-vector hvec))
      (set-heap-hvec! aheap hvec)))
  ; Percolate toward the leaves, either with the last element of the heap,
  ; or with the provided element v.
  (when (> N 0)
    (define new-pos
      (let loop ([pos 1])
        (cond
          [(or (= pos N)
               (> (* 2 pos) N))
           pos]
          [else
           (define pos2 (* pos 2))
           (define pos3 (+ 1 pos2))
           (define v2 (vector-ref hvec pos2))
           (define v3 (vector-ref hvec pos3))
           (define-values (posmin vmin)
             (if (or (> pos3 N)
                     (<? v2 v3))
                 (values pos2 v2)
                 (values pos3 v3)))
           #;(debug-vars N pos v pos2 v2 pos3 v3 posmin vmin heap)
           (cond [(<? vmin v)
                  ;; Replace the current position with the child
                  (vector-set! hvec pos vmin)
                  (loop posmin)]
                 [else pos])])))
    (vector-set! hvec new-pos v))
  v-root)

(module+ test
  (require rackunit)
  (displayln "Running tests.")

  (define H (make-heap <))
  (define l
    #;'(38 44 38 69 73 94 60 82)
    (build-list (+ 1 (random 100)) (λ(i)(random 100))))
  (for-each (λ(x)(heap-insert! H x)) l)
  (define l2
    (for/list ([i (in-range (length l))])
      (heap-remove-min! H)))

  (define lsorted (sort l <))
  (check-equal? lsorted l2)

  (for-each (λ(x)(heap-insert! H x)) l)
  (define maxl (apply max l))
  (define l3 (build-list (length l) (λ(i)(+ maxl 1 (random 100))))) ; make sure number are larger than in l1, otherwise numbers in l3 could be removed also.
  (for ([x (in-list l3)])
    (heap-remove-min! H x))
  (define l4 (for/list ([i (in-range (length l3))])
               (heap-remove-min! H)))
  (define l3-sorted (sort l3 <))
  (check-equal? l3-sorted l4)
  )

