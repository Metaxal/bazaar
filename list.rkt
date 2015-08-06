#lang racket/base
;;; Copyright (C) Laurent Orseau, 2010-2013
;;; GNU Lesser General Public Licence (http://www.gnu.org/licenses/lgpl.html)

(require racket/list
         racket/contract
         syntax/parse/define)

(provide (all-defined-out))

(module+ test
  (require "rackunit.rkt"))

(define (choose l)
  (list-ref l (random (length l))))

(define (transpose ll)
  (if (empty? ll)
      ll
      (apply map list ll)))

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

;;; These should be in values.rkt instead

;; Return the multiple values of proc-call as a list
;; See also values->list in "values.rkt"
(define-syntax-rule (call/values->list expr)
  (call-with-values (λ()expr) (λ l l)))
; Example:
; (call/values->list (values 1 2 3))
; -> '(1 2 3)

(define-simple-macro (define-list (var:id ...) e:expr)
  (define-values (var ...) 
    (apply values e)))
; Example:
; (define-list (a b c) (list 1 2 3))

(define-simple-macro (let-list ([var:id ... e:expr] ...) body ...)
  (let-values ([(var ...) (apply values e)] ...) body ...))
; Example:
; (let-list ([x y z '(a b c)])
;   (list z y x))
;-> '(c b a)

;; Replaces the last element of l by x.
;; If l is empty, the empty list is returned.
(define/contract (replace-last l x)
  (list? any/c . -> . list?)
  (cond [(null? l) '()]
        [(null? (cdr l)) (list x)]
        [else (cons (car l) (replace-last (cdr l) x))]))

(module+ test
  (check-equal? (replace-last '() 'a) '())
  (check-equal? (replace-last '(a b c) 'a) '(a b a)))

(define/contract (remove-last l)
  ((and/c list? (not/c empty?)) . -> . list?)
  (cond [(null? l) '()]
        [(null? (cdr l)) '()]
        [else (cons (car l) (remove-last (cdr l)))]))

(module+ test
  (check-fail (remove-last '()))
  (check-equal? (remove-last '(a b c)) '(a b)))