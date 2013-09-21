#lang racket/base
;;; Copyright (C) Laurent Orseau, 2010-2013
;;; GNU Lesser General Public Licence (http://www.gnu.org/licenses/lgpl.html)

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
