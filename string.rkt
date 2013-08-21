#lang racket/base
;;; Copyright (C) Laurent Orseau, 2010-2013
;;; GNU Lesser General Public Licence (http://www.gnu.org/licenses/lgpl.html)

(provide (all-defined-out))

(define (string?-ci=? a b)
  (and (string? a) (string? b)
       (string-ci=? a b)))

(define (string-reverse str)
  (list->string (reverse (string->list str))))

