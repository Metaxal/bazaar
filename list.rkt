#lang racket/base
;;; Copyright (C) Laurent Orseau, 2010-2013
;;; GNU Lesser General Public Licence (http://www.gnu.org/licenses/lgpl.html)

(require racket/list
         syntax/parse/define)

(provide (all-defined-out))

(module+ test
  (require rackunit))

(define (choose l)
  (list-ref l (random (length l))))

(define (transpose ll)
  (apply map list ll))

(module+ test
  (check-equal? (transpose '((a b c) (1 2 3)))
                '((a 1) (b 2) (c 3))))

(define (consr x l)
  (append l (list x)))

;; Like add-between, but after each element
(define (add-after l elt)
  (append-map (λ(e)(list e elt)) l))

(define (add-before l elt)
  (append-map (λ(e)(list elt e)) l))

;; Return the multiple values of proc-call as a list
(define-syntax-rule (call/values->list expr)
  (call-with-values (λ()expr) (λ l l)))
; Example:
; (call/values->list (values 1 2 3))
; -> '(1 2 3)

(define-simple-macro (define-list (var:id ...) e:expr)
  (define-values (var ...) 
    (apply values expr)))
; Example:
; (define-list (a b c) (list 1 2 3))

(define-simple-macro (let-list ([var:id ... e:expr] ...) body ...)
  (let-values ([(var ...) (apply values e)] ...) body ...))
; Example:
; (let-list ([x y z '(a b c)])
;   (list z y x))
;-> '(c b a)


