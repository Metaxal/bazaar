#lang racket/base

(require define2
         syntax/parse/define
         racket/list)

(provide (all-defined-out))

;; Increments the variable
(define-simple-macro (++ var:id)
  (set! var (add1 var)))

;; Increments the variable and returns the new value
(define-simple-macro (++@ var:id)
  (begin
    (set! var (add1 var))
    var))

;; Increments the variable and returns the old value
(define-simple-macro (@++ var:id)
  (begin0
    var
    (set! var (add1 var))))

(define-simple-macro (-- var:id)
  (set! var (sub1 var)))

(define-simple-macro (+= var:id n:expr ...)
  (set! var (+ var n ...)))

(define-simple-macro (-= var:id n:expr ...)
  (set! var (- var n ...)))

(define-simple-macro (cons! val:expr var:id)
  (set! var (cons val var)))

(define-simple-macro (rest! var:id)
  (set! var (rest var)))

(define-simple-macro (append! l:id l2:expr ...)
  (set! l (append l l2 ...)))

(define-simple-macro (append2! l:id l2)
  (set! l2 (append l l2)))

;;; Strings

(define-simple-macro (strappend! str:id s ...)
  (set! str (string-append str s ...)))

(define-simple-macro (surround! l str:id r)
  (set! str (string-append l str r)))

;;; General case

(define-simple-macro (update! var:id proc:expr)
  (set! var (proc var)))
