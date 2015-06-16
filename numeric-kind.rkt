#lang racket/base

(require racket/dict
         math/flonum
         racket/fixnum
         racket/extflonum
         math/bigfloat
         racket/unsafe/ops)

(provide use-numeric-kind)

(define op-dict
  (list
   (list '$+   +   fx+         fl+   fl2+   extfl+   bf+   unsafe-fx+         unsafe-fl+   unsafe-extfl+)
   (list '$-   -   fx-         fl-   fl2-   extfl-   bf-   unsafe-fx-         unsafe-fl-   unsafe-extfl-)
   (list '$*   *   fx*         fl*   fl2*   extfl*   bf*   unsafe-fx*         unsafe-fl*   unsafe-extfl*)
   (list '$/   /   fxquotient  fl/   fl2/   extfl/   bf/   unsafe-fxquotient  unsafe-fl/   unsafe-extfl/)
   (list '$>   >   fx>         fl>   fl2>   extfl>   bf>   unsafe-fx>         unsafe-fl>   unsafe-extfl>)
   (list '$<   <   fx<         fl<   fl2<   extfl<   bf<   unsafe-fx<         unsafe-fl<   unsafe-extfl<)
   (list '$>=  >=  fx>=        fl>=  fl2>=  extfl>=  bf>=  unsafe-fx>=        unsafe-fl>=  unsafe-extfl>=)
   (list '$<=  <=  fx<=        fl<=  fl2<=  extfl<=  bf<=  unsafe-fx<=        unsafe-fl<=  unsafe-extfl<=)
   ; to complete...
   ))

(define (get-op op-sym kind)
  (define idx
    (case kind
      [(default generic #f)             0]
      [(fixnum)                         1]
      [(flonum fl double)               2]
      [(flonum2 fl2 double-double)      3]
      [(extfl extflonum)                4]
      [(bigfloat)                       5]
      [(unsafe-fx unsafe-fixnum)        6]
      [(unsafe-fl unsafe-flonum)        7]
      [(unsafe-extfl unsafe-extflonum)  8]))
  (list-ref (dict-ref op-dict op-sym) idx))

(define-syntax-rule (use-numeric-kind num-kind (op ...) body ...)
  (let ([op (get-op 'op num-kind)] ...)
    body ...))
