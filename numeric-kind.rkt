#lang racket/base

(require racket/dict
         math/flonum
         racket/fixnum
         racket/extflonum
         math/bigfloat
         racket/unsafe/ops)

(provide with-numeric-kind
         with-current-numeric-kind
         current-numeric-kind
         [rename-out [get-op $op]])

(module+ test
  (require rackunit))

(define current-numeric-kind
  (make-parameter
   'real
   (λ(kind) (if (numeric-kind-index kind)
                kind
                (error 'current-numeric-kind
                       "Unrecognized numeric kind: ~a. Existing kinds are ~a\n"
                       kind
                       (dict-ref op-dict '$kind))))))

;; In case we receive a float, convert it.
(define (fx x)
  (fl->fx (fl x)))

(define (unsafe-fx x)
  (unsafe-fl->fx (fl x)))

(define op-dict
  (list
   (list '$kind  'real   'fixnum     'flonum  'extflonum      'bigfloat         'unsafe-fixnum     'unsafe-flonum  'unsafe-extflonum) ; $kind holds the symbolic kind
   (list '$?     real?   fixnum?     flonum?  extflonum?      bigfloat?         fixnum?            flonum?         extflonum?)
   (list '$      values  fx          fl       real->extfl     bf                unsafe-fx          fl              real->extfl) ; real->$
   (list '$->    values  fx->fl      values   extfl->inexact  bigfloat->flonum  unsafe-fx->fl      values          extfl->inexact) ; $->real
   (list '$+     +       fx+         fl+      extfl+          bf+               unsafe-fx+         unsafe-fl+      unsafe-extfl+)
   (list '$-     -       fx-         fl-      extfl-          bf-               unsafe-fx-         unsafe-fl-      unsafe-extfl-)
   (list '$*     *       fx*         fl*      extfl*          bf*               unsafe-fx*         unsafe-fl*      unsafe-extfl*)
   (list '$/     /       fxquotient  fl/      extfl/          bf/               unsafe-fxquotient  unsafe-fl/      unsafe-extfl/)
   (list '$=     =       fx=         fl=      extfl=          bf=               unsafe-fx=         unsafe-fl=      unsafe-extfl=)
   (list '$>     >       fx>         fl>      extfl>          bf>               unsafe-fx>         unsafe-fl>      unsafe-extfl>)
   (list '$<     <       fx<         fl<      extfl<          bf<               unsafe-fx<         unsafe-fl<      unsafe-extfl<)
   (list '$>=    >=      fx>=        fl>=     extfl>=         bf>=              unsafe-fx>=        unsafe-fl>=     unsafe-extfl>=)
   (list '$<=    <=      fx<=        fl<=     extfl<=         bf<=              unsafe-fx<=        unsafe-fl<=     unsafe-extfl<=)
   (list '$abs   abs     fxabs       flabs    extflabs        bfabs             unsafe-fxabs       unsafe-flabs    unsafe-extflabs)
   (list '$min   min     fxmin       flmin    extflmin        bfmin             unsafe-fxmin       unsafe-flmin    unsafe-extflmin)
   ; to complete...
   ))

(define (subindex e l [= equal?])
  (let loop ([l l] [idx 0])
    (cond [(null? l) #f]
          [(member e (car l) =) idx]
          [else (loop (cdr l) (+ idx 1))])))

(define numeric-kinds
  '((default generic real #f)
    (fixnum fx)
    (flonum fl)
    (extflonum extfl)
    (bigfloat bf)
    (unsafe-fixnum unsafe-fx)
    (unsafe-flonum unsafe-fl)
    (unsafe-extflonum unsafe-extfl)
    ))

(define (numeric-kind-index kind)
  (subindex kind numeric-kinds))

(define (existing-numeric-kinds)
  (dict-ref op-dict '$kind))

(define (get-op op-sym [kind (current-numeric-kind)])
  (define idx (numeric-kind-index kind))
  (unless idx
    (error 'get-op "Could not find numeric kind: ~a. Existing kinds: ~a"
           kind
           (existing-numeric-kinds)))
  (list-ref (dict-ref op-dict op-sym
                      (λ()(error 'get-op
                                 "Operator ~a not found. Existing operators: ~a"
                                 op-sym
                                 (dict-keys op-dict))))
            idx))

(define-syntax-rule (with-current-numeric-kind (op ...) body ...)
  (let ([op (get-op 'op)] ...)
      body ...))

(define-syntax-rule (with-numeric-kind num-kind (op ...) body ...)
  (parameterize ([current-numeric-kind num-kind])
    (with-current-numeric-kind (op ...) body ...)))

;; No need for a "current" parametrized version?
#;
(define-syntax-rule (define-for-numeric-kind (op ...))
  (begin (define op (get-op 'op)) ...))

(module+ test
  (define (test-kind kind)
    (with-numeric-kind
     kind ($kind $ $? $= $+ $- $* $/ $> $< $>= $<=)
     (check-eq? $kind kind)
     (define n ($* ($/ ($- ($+ ($ 3) ($ 2)) ($ 15))
                       ($ 2))
                   ($ 4)))
     (check-pred $? n)
     (check $= n ($ -20))))
  (for ([kind (existing-numeric-kinds)])
    (test-kind kind))

  (check-equal?
   (with-numeric-kind
    'bigfloat ($ $+)
    ($+ ($ 3) ($ 4)))
   (bf 7))

  )

;; TODO:
;; - Can modify the '$' prefix? (something like '_' can be more readable sometimes)
;;   (also to avoid collision with other modules)
;; - define-numeric-kind to avoid the indentation shift ?