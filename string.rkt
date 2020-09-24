#lang racket/base
;;; Copyright (C) Laurent Orseau, 2010-2013
;;; GNU Lesser General Public Licence (http://www.gnu.org/licenses/lgpl.html)

(require racket/port
         racket/format
         racket/list)

(provide (all-defined-out))

(module+ test
  (require rackunit))

(define (string?-ci=? a b)
  (and (string? a) (string? b)
       (string-ci=? a b)))

(define (string-reverse str)
  ; Not efficient...
  (list->string (reverse (string->list str))))

(define (string->string-or-false s [false-string ""])
  (and (not (equal? s false-string)) s))

(define (string-or-false->string s [false-string ""])
  (or s false-string))

;; Returns the lisp-like data contained in a string.
;; When a read exception is caught, if on-error is a procedure of arity 1,
;; it is called with the exception as argument, o.w. it is returned as a value.
(define (string->data str #:on-error [on-error (λ (e) (raise e))])
  (with-handlers ([exn:fail:read? (if (and (procedure? on-error)
                                           (procedure-arity-includes? on-error 1))
                                      on-error
                                      (λ (e) on-error))])
    (call-with-input-string str (λ (in) (port->list read in)))))

(module+ test
  (check-equal? (string->data "((a)((\"bc\")(d . 5))) #(1 2 3)")
                (list '((a) (("bc") (d . 5)))
                      #(1 2 3)))
  (check-false (string->data " . " #:on-error #f))
  (check-false (string->data " # " #:on-error #f))
  (check-exn exn:fail:read? (λ () (string->data " # ")))
  (check-exn exn:fail:read? (λ () (string->data " . "))))

;; Returns a list of strings that are the cartesian product of the arguments.
(define (string*
         #:->string [->string ~a]
         . args)
  (let loop ([args args])
    (cond
      [(empty? args) '("")]
      [else
       (define x (first args))
       (for*/list ([y (in-list (if (list? x) x (list x)))]
                   [str (in-list (loop (rest args)))])
         (string-append (->string y) str))])))
(module+ test
  (check-equal? (string*) '(""))
  (check-equal? (string* "_" '(a b c) ":" '(1 2 3))
                '("_a:1" "_a:2" "_a:3" "_b:1" "_b:2" "_b:3" "_c:1" "_c:2" "_c:3")))


