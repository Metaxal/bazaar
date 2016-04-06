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
  (check-equal? (transpose '()) '())
  (check-equal? (transpose '((a b c) (1 2 3) (d e f)))
                '((a 1 d) (b 2 e) (c 3 f))))

(define (consr x l)
  (append l (list x)))

;; Like add-between, but after each element
(define (add-after l elt)
  (append-map (λ(e)(list e elt)) l))

(define (add-before l elt)
  (append-map (λ(e)(list elt e)) l))

(define (list->cumul-list l)
  (define sum 0)
  (for/list ([x (in-list l)])
    (set! sum (+ sum x))
    sum))

(module+ test
  (check-equal?
   (list->cumul-list '(2 8 3 5 5 0 -1 2))
   '(2 10 13 18 23 23 22 24)))

;; l: (listof real?)
;; -> (listof real?)
(define (normalize l)
  (define s (apply + l))
  (map (λ(x)(/ x s)) l))

;; Maps a list of lists of elements (keeps the list of list structure).
;; See also tree-map in tree.rkt
(define (map-map proc ll)
  (for/list ([l (in-list ll)])
    (for/list ([x (in-list l)])
      (proc x))))

(module+ test
  (check-equal?
   (map-map add1 '((0 1) (2 3) (5 4)))
   '((1 2) (3 4) (6 5))))

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
