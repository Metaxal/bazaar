#lang racket/base

(require (for-syntax racket/base syntax/parse))

(provide define/memoize
         memoize
         define/memoize/values
         memoize/values
         cache-last)

(require define2)

;;; If using eq? we could use a weak-hasheq, but that means
;;; all arguments must be checked with eq? AND they must
;;; be held in a list of arguments that is itself eq?, which
;;; is unlikely :(

(define (memoize f)
  (define h (make-hash))
  (λ args
    (hash-ref! h args (λ () (apply f args)))))

;; Simple memoization
(define-syntax-rule (define/memoize (f args ...) body ...)
  (begin
    ; One hash per function
    (define h (make-hash))
    (define (f args ...)
      (hash-ref! h (list args ...) (λ()body ...)))))

(module+ test
  (require rackunit)
  
  (define foo
    (memoize (λ(x y)(+ (random) x y))))
  
  (define/memoize (bar x y)
    (+ (random) x y))
  
  (check = (foo 2 3) (foo 2 3))
  (check = (bar 4 5) (bar 4 5)))

;; More general form using values, but likely slower, unless the JIT is smart enough?
(define (memoize/values f)
  (define h (make-hash))
  (λ args
    (apply values (hash-ref! h args (λ () (call-with-values (λ () (apply f args)) list))))))

;; Simple memoization
(define-syntax-rule (define/memoize/values (f args ...) body ...)
  (begin
    ; One hash per function
    (define h (make-hash))
    (define (f args ...)
      (apply values (hash-ref! h (list args ...) (λ () (call-with-values (λ () body ...) list)))))))

;; Todo: Throw an exception when a cycle is detected?

;; A simple form a memoization where if the syntax element is called with the same (eq?)
;; arguments, then the result is returned immediately without calculating it.
;; Note that the same computation at two different syntax location make two separate computations.
(define-syntax (cache-last stx)
  (syntax-parse stx
    [(_ (fun args ...))
     #:with cache-val (syntax-local-lift-expression #'#f)
     #:with cache-args (syntax-local-lift-expression #''())
     #'(let ([largs (list args ...)])
         (cond [(and (= (length cache-args) (length largs))
                     (for/and ([c (in-list cache-args)] [a (in-list largs)])
                       (eq? c a)))
                cache-val]
               [else
                (define v (apply fun largs))
                (set! cache-val v)
                (set! cache-args largs)
                v]))]))


(module+ stress-test
  (define l (build-list 100000 (λ (i) (random))))
  ;; Extremely fast
  (time
   (for/sum ([i 100000])
     (cache-last (length l))))

  ;; Slow because no caching
  (time
   (for/sum ([i 100000])
     (length l)))

  ;; Slow because can't reuse the previous cache
  (time
   (for/sum ([i 100000])
     (define l (build-list 100000 (λ (i) (random))))
     (cache-last (length l)))))
