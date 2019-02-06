#lang racket/base
;;; Copyright (C) Laurent Orseau, 2010-2013
;;; GNU Lesser General Public Licence (http://www.gnu.org/licenses/lgpl.html)

(require racket/port)

(provide (all-defined-out))

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

;; Returns the lisp-like data contained in a string
(define (string->data str)
  (call-with-input-string
   str
   (λ(in) (let loop ([result '()])
            (let ([v (read in)])
              (if (eof-object? v)
                  (reverse result)
                  (loop (cons v result))))))))
