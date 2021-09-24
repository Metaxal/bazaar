#lang racket/base

(require (for-syntax racket/base
                     racket/syntax)
         syntax/parse/define)

(provide (all-defined-out))

(module+ test (require racket "rackunit.rkt" "values.rkt"))

(define-syntax-rule (while test body ...)
  (let loop ()
    (when test
      body ...
      (loop))))

(define-syntax-rule (until test body ...)
  (let loop ()
    (unless test
      body ...
      (loop))))

(define no-elt (gensym 'no-elt))


;; for/fold is very often used with define-values
;; todo: add a finalizer that applies to the value before being set to the variable.
;; useful for lists that often are reversed.
(define-syntax-rule (define/for/fold ([x a] ...) (y ...) body ...)
  (define-values (x ...)
    (for/fold ([x a] ...) (y ...)
      body ...)))

;; Use the #:when #t thing for more flexibility
#;(define-syntax-rule (define/for*/fold ([x a] ...) (y ...) body ...)
  (define-values (x ...)
    (for*/fold ([x a] ...) (y ...)
      body ...)))

#; ; Ex:
(begin
  (for/fold/define 
   ([l '()] [sum 0])
   ([i 10])
   (define r (random 1000))
   (values (cons (list i r) l) (+ sum r)))
  (list l sum))

;; From the docs on for/fold/derived
(define-syntax (for/max stx)
  (syntax-case stx ()
    [(_ clauses body ... tail-expr)
     (with-syntax ([original stx])
       #'(for/fold/derived original
                           ([current-max -inf.0])
                           clauses body ...
                           (define maybe-new-max tail-expr)
                           (if (> maybe-new-max current-max)
                               maybe-new-max
                               current-max)))]))

(define-syntax (for/min stx)
  (syntax-case stx ()
    [(_ clauses body ... tail-expr)
     (with-syntax ([original stx])
       #'(for/fold/derived original
                           ([current-max +inf.0])
                           clauses body ...
                           (define maybe-new-max tail-expr)
                           (if (< maybe-new-max current-max)
                               maybe-new-max
                               current-max)))]))

;; Like for, but returns the best element and its value when elements are compared with <?.
;; Each iteration must return the current element and its value (in this order).
;; Returns the best value and the corresponding best element.
(define-syntax (for/best stx)
  (syntax-case stx ()
    [(_ <? clauses body ... tail-expr)
     (with-syntax ([original stx])
       #'(for/fold/derived original
                           ([best-elt no-elt] [best-val no-elt])
                           clauses body ...
                           (define-values (new-elt new-val) tail-expr)
                           (if (or (eq? best-elt no-elt)
                                   (<? new-val best-val))
                               (values new-elt  new-val)
                               (values best-elt best-val))))]))

#;(define-syntax-rule (for/best <? (bindings ...) body ...)
  (for/fold ([best-elt no-elt] [best-val no-elt]) 
    (bindings ...)
    (let-values ([(new-elt new-val) 
                  (let () body ...)])
      ;(displayln (list best-elt best-val (eq? best-elt no-elt)))
      (if (or (eq? best-elt no-elt)
              (<? new-val best-val))
          (values new-elt  new-val)
          (values best-elt best-val)))))

(module+ test
  (check-equal?
   (values->list (for/best < ([x '(-3 3 4 8 -5 2 10)]) 
                           (values x x)))
   (list -5 -5))
  
  (check-equal?
   (values->list (for/best > ([x '((a . 1)(b . 5)(c . 3))]) 
                           (values x (cdr x))))
   '((b . 5) 5))
  
  (check-equal?
   (values->list (for/best > ([x '("abc" "abcdef" "abcd")])
                           (values x (string-length x))))
   '("abcdef" 6))
  
  ; must not raise a "defined not allowed in definition context" exception
  (check-not-fail 
   (values->list
    (for/best < ([x '(1)])
              (define y (- 3 x))
              (values x y))))
  )


;; SUPERSEDED by `in-zip`.
;; Zips the list l from left to right. At each step of the loop, the bindings
;; rev-left, x and next-right.
;; Accepts any number of fold (for/fold-like) variables. The number of values returned
;; by the body must match the number of fold variables.
;; rev-left is in reverse order to avoid the cost of reversing if it is not necessary
;; (by contrast to for/list).
;; if the break expression is not #f, the loop terminates immediately and the return values are
;; returned.
;; If #:result is used, it becomes the return value when the loop terminates. (The res ... bindings
;; are available.)
;; `zip-loop` is tail-recursive.
;; If the rev-left binding is not provided, it is also not constructed for efficiency.
;; (this is then not exactly zipping, put provides a consistent interface.)
;; If no fold variables are used, `zip-loop` returns void.
;; Note: In DrRacket, do Edit > Preferences > Editing > Indenting, in Lambda-like keywords,
;; add zip-loop.
(define-syntax (zip-loop stx)
  (syntax-parse stx
    [(_ ([((~optional rev-left1:id) x1:id right1:id) l1:expr]
         [res:id v0:expr] ...
         (~alt (~optional (~seq #:result result:expr)
                          #:defaults ([result #'(values res ...)]))
               (~optional (~seq #:break break:expr)))
         ...)
        body ...)
     #`(let loop ((~? (~@ [rev-left1 '()]))
                  [pre-right1 l1]
                  [res v0] ...)
         (if (null? pre-right1)
             result
             (let ([x1 (car pre-right1)]
                   [right1 (cdr pre-right1)])
               #,(with-syntax* ([let-vals-bindings (if (null? (syntax-e #'(res ...)))
                                                       #'([() (let () body ... (values))])
                                                       #'([(res ...) (let () body ...)]))]
                                [do-loop #'(let-values let-vals-bindings
                                             (loop (~? (~@ (cons x1 rev-left1))) right1
                                                   res ...))])
                   (if (attribute break)
                       #'(let ([break-val break])
                           (if break
                               result
                               do-loop))
                       #'do-loop)))))]))

(module+ test
  (let ([l (range 10)] [resl '(8 6 4 2 0)])
    (check-equal?
     (zip-loop ([(rl x r) l] [res '()])
       (if (even? x) (cons x res) res))
     resl)
    
    (check-equal?
     (zip-loop ([(x r) l] [res '()])
       (if (even? x) (cons x res) res))
     resl)
    
    (check-equal?
     (let ([res '()])
       (zip-loop ([(x r) l])
         (when (even? x) (set! res (cons x res))))
       res)
     resl)
    
    (check-equal?
     (let ([res '()])
       (zip-loop ([(lr x r) l])
         (when (even? x) (set! res (cons x res))))
       res)
     resl)
    
    (check-equal?
     (zip-loop ([(rl x r) l]
                [res '()]
                [i 0]
                #:break (= i 5)
                #:result (list i res))
       (values (if (even? x) (cons x res) res)
               (+ i 1)))
     '(5 (4 2 0)))
    
    (check-equal?
     (zip-loop ([(rl x r) l]
                [res '()]
                [i 0]
                #:result (list i res)
                #:break (= x 5))
       (values (if (even? x) (cons x res) res)
               (+ i 1)))
     '(5 (4 2 0)))
    ))

; Example. `res` and `i` are for/fold-like variables
#;#;#;
(zip-loop ([(rl x r) '(a b c d e)] [res '()] [i 0])
  (values (cons (list i ': rl x r) res)
          (+ i 1)))
'((4 : (d c b a) e ())
  (3 : (c b a) d (e))
  (2 : (b a) c (d e))
  (1 : (a) b (c d e))
  (0 : () a (b c d e)))
5

;; Note: `rev-append` (bazaar/list) is particularly useful to do (rev-append rev-left right)
;; and restore the original sequence while avoid unnecessary reversals.
;; Ex: All subsets of (range 10) with only a single element missing:
#;(zip-loop ([(rl x r) (range 10)] [res '()])
    (cons (rev-append rl r)
          res)) 
;; Same with 2 elements missing:
#;(zip-loop ([(rl x r) (range 10)] [res '()])
    (zip-loop ([(rl2 x2 r2) r] [res res])
      (cons (rev-append rl (rev-append rl2 r2))
            res)))


(module+ test
  (require racket/list)

  (let ([the-list (range 10)])
    (check-equal?
     (values->list
      (zip-loop ([(rl x r) the-list] [z1 '()] [z2 0])
                (values (cons x z1)
                        (+ x z2))))
     (list (reverse the-list)
           (apply + the-list))))
  
  (check-equal?
   (zip-loop ([(rl x r) '()] [res 'a])
             3)
   'a)

  (check equal?
         (values->list
          (zip-loop ([(rl x r) (range 5)])
            'a))
         '()) ; no values
  (check-equal?
   (let ([s 0])
     (zip-loop ([(rl x r) (range 5)])
               (set! s (+ s x)))
     s)
   (apply + (range 5)))
  )


(module+ main
  (require racket)
  (displayln "zip-loop example vs combinations")
  (define l (range 2000))

  ; generate all pairs of unordered indices
  ; (that is, (x_1 x_2) is produced, but not (x_2 x_1))
  (collect-garbage)(collect-garbage)
  ; 366ms for l=(range 2000) (fast!)
  ; fast, but almost everything is garbage collection
  (time
   (length
    (zip-loop ([(x r) l] [res '()])
      (zip-loop ([(x2 r2) r] [res2 res])
        (cons (list x x2) res2)))))
  
  (collect-garbage)(collect-garbage)
  ; 397ms for l=(range 2000)
  ; same as above
  (time
   (length
    (zip-loop ([(rl x r) l] [res '()])
      (zip-loop ([(rl2 x2 r2) r] [res2 res])
        (cons (list x x2) res2)))))

  (collect-garbage)(collect-garbage)
  ; 63644ms for l=(range 2000)
  ; very slow, but no garbage collection in proportion.
  (time
   (length (combinations l 2))))

;; Similar to zip-loop, but usable in for loops.
;; This is as efficient as a named let.
(define-sequence-syntax in-list+rest
  (lambda () #'in-list+rest/proc)
  (lambda (stx)
    (syntax-case stx ()
      [[(x r) (_ lst)]
       #'[(x r)
          (:do-in
           ([(l) lst])
           (unless (list? l)
             (raise-argument-error 'in-list+rest "list?" l))
           ([l l])
           (not (null? l))
           ([(x r) (values (car l) (cdr l))])
           #true
           #true
           [r])]]
      [_ #f])))

(define (in-list+rest/proc l)
  (for/list ([(x r) (in-list+rest l)]) (list x r)))

(module+ test
  (check-equal? (for/list ([(x r) (in-list+rest '(a b c))])
                  (cons x r))
                '((a b c) (b c) (c)))
  (check-equal? (for*/list ([(x r) (in-list+rest '(a b c))]
                            [y (in-list r)])
                  (list x y))
                '((a b) (a c) (b c))))

;; Similar to zip-loop, but usable in for loops.
;; This is as efficient as a named let.
(define-sequence-syntax in-zip
  (lambda () #'in-zip/proc)
  (lambda (stx)
    (syntax-case stx ()
      [[(rev-l x r) (_ lst)]
       #'[(rev-l x r)
          (:do-in
           ; ([(outer-id ...) outer-expr] ...)
           ([(l) lst])
           ; outer-check
           (unless (list? l)
             (raise-argument-error 'in-list+rest "list?" l))
           ; ([loop-id loop-expr] ...)
           ([rev-l '()] [r l])
           ; pos-guard
           (not (null? r))
           ;([(inner-id ...) inner-expr] ...)
           ([(rev-l x r) (values rev-l (car r) (cdr r))])
           ; pre-guard
           #true
           ; post-guard
           (not (null? r))
           ; (loop-arg ...)
           [(cons x rev-l) r])]]
      [_ #f])))

(define (in-zip/proc l)
  (for/list ([(rev-l x r) (in-zip l)]) (list rev-l x r)))

(module+ test
  (require rackunit)
  (check-equal?
   (for/list ([(rev-l x r) (in-zip '(a b c))])
     (list rev-l x r))
   '((() a (b c))
     ((a) b (c))
     ((b a) c ())))
  (check-equal?
   (for/list ([(rev-l x r) (in-zip '())])
     (list rev-l x r))
   '()))
