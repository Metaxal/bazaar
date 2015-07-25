#lang racket/base
;;; Copyright (C) Laurent Orseau, 2010-2013
;;; GNU Lesser General Public Licence (http://www.gnu.org/licenses/lgpl.html)

(require (for-syntax racket/base))

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
(define-syntax-rule (define/for/fold ([x a] ...) (y ...) body ...)
  (define-values (x ...)
    (for/fold ([x a] ...) (y ...)
      body ...)))

#; ; Ex:
(begin
  (for/fold/define 
   ([l '()] [sum 0])
   ([i 10])
   (define r (random 1000))
   (values (cons (list i r) l) (+ sum r)))
  (list l sum))

;; Like for, but returns the best element and its value when elements are compared with <?
;; Each iteration must return the current element and its value (in this order).
(define-syntax-rule (for/best <? (bindings ...) body ...)
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
