#lang racket/base

(require (for-syntax racket/base syntax/parse))

(provide define/memoize
         memoize
         define/memoize/values
         memoize/values
         cache-last)

(require define2)

;;; NOTICE: Tests are based on `eq?`, so if a value is mutated
;;; the memoization functions may return the wrong value.


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

;; faster than (andmap eq? cache-args largs) 
(define (fast-andmap2-eq? cs as)
  (let loop ([cs cs] [as as])
    (cond [(and (null? cs) (null? as))]
          [(or (null? cs) (null? as))
           #false]
          [(not (and (pair? cs) (pair? as)))
           (eq? cs as)]
          [(eq? (car cs) (car as))
           (loop (cdr cs) (cdr as))]
          [else
           #false])))

;; A simple form a memoization where if the syntax element is called with the same (eq?)
;; arguments, then the result is returned immediately without calculating it.
;; Note that the same computation at two different syntax locations make two separate computations.
(define-syntax (cache-last stx)
  (syntax-parse stx
    [(_ (fun args ...))
     #:with cache-val  (syntax-local-lift-expression #'#f)
     #:with cache-args (syntax-local-lift-expression #''())
     #'(let ([largs (list args ...)])
         (cond [(fast-andmap2-eq? cache-args largs)
                cache-val]
               [else
                (define v (apply fun largs))
                (set! cache-val v)
                (set! cache-args largs)
                v]))]))

(define-syntax-rule (apply/cache-last f arg ...)
  (cache-last (f arg ...)))

(module+ test
  ;; Make sure that the call is syntax-location specific
  (check-equal? (let ([a 'a] [b 'b])
                  (for/list ([x 3] [y 3])
                    (list (cache-last (list a))
                          (cache-last (list b)))))
                '(((a) (b)) ((a) (b)) ((a) (b)))))

;; racket -l racket/init -e '(require (submod bazaar/memoize stress-test))'
(module+ stress-test
  (require bazaar/debug)
  (define N 100000)
  (define l (build-list N (λ (i) (random))))
  
  ;; Extremely fast because 
  (time
   (for/sum ([i N])
     ;; Just to make sure `cache-last` caches values per syntax point
     (assert (equal? (cache-last (+ 3 2)) 5))
     (if (equal? (cache-last (length l)) N) 1 0)))

  ;; Slow because l2 is not `eq?` from one iteration to the next
  ;; (unless the compiler is smart enough, which it currently isn't).
  (time
   (for/sum ([i N])
     (define l2 (cons 'a l))
     (if (= (cache-last (length l2)) (+ N 1)) 1 0)))

  ;; Slow because no caching
  (time
   (for/sum ([i N])
     (if (= (length l) N) 1 0))))
