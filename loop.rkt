#lang racket/base
;;; Copyright (C) Laurent Orseau, 2010-2013
;;; GNU Lesser General Public Licence (http://www.gnu.org/licenses/lgpl.html)

(require (for-syntax racket/base)
         syntax/parse/define)

(provide (all-defined-out))

(module+ test (require "rackunit.rkt" "values.rkt"))

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


;; Zips the list l from left to right. At each step of the loop, the bindings
;; rev-left, x and next-right 
(define-simple-macro (zip-loop ([(rev-left:id x:id next-right:id) l:expr]
                                [res:id v0:expr] ...)
                               body ...)
  (let loop ([rev-left '()] [right l] [res v0] ...)
    (if (null? right)
        (values res ...)
        (let ([x (car right)]
              [next-right (cdr right)])
          (let-values ([(res ...) (let () body ...)])
            (loop (cons x rev-left) next-right res ...))))))

; Example
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

(module+ test
  (require racket/list)
  (let ([the-list (range 10)])
    (check-equal?
     (values->list
      (zip-loop ([(rl x r) the-list] [z1 '()] [z2 0])
                (values (cons x z1)
                        (+ x z2))))
     (list (reverse the-list)
           (apply + the-list)))))


(module+ main
  (require racket)
  (define l (range 1000))

  ; generate all pairs of unordered indices
  ; (that is, (x_1 x_2) is produced, but not (x_2 x_1))
  (collect-garbage)(collect-garbage)
  ; 56ms for l=(range 1000) (fast!)
  (time
   (length
    (zip-loop ([(rl x r) l] [res '()])
              (zip-loop ([(rl2 x2 r2) r] [res2 res])
                        (cons (list x x2) res2)))))

  (collect-garbage)(collect-garbage)
  ; 8500ms (!!)
  (time
   (length (combinations l 2))))

