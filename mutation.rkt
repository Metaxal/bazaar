#lang racket/base
;;; Copyright (C) Laurent Orseau, 2010-2013
;;; GNU Lesser General Public Licence (http://www.gnu.org/licenses/lgpl.html)

(require syntax/parse/define
         racket/list)

(provide (all-defined-out))

(define-simple-macro (++ var:id)
  (set! var (add1 var)))

(define-simple-macro (-- var:id)
  (set! var (sub1 var)))

(define-simple-macro (+= var:id n)
  (set! var (+ var n)))

(define-simple-macro (-= var:id n)
  (set! var (- var n)))

(define-simple-macro (cons! val:expr var:id)
  (set! var (cons val var)))

(define-simple-macro (rest! var:id)
  (set! var (rest var)))

(define-simple-macro (append! l:id l2)
  (set! l (append l l2)))

(define-simple-macro (append2! l:id l2)
  (set! l2 (append l l2)))

;;; Strings

(define-simple-macro (strappend! str:id s ...)
  (set! str (string-append str s ...)))

(define-simple-macro (surround! l str:id r)
  (set! str (string-append l str r)))
