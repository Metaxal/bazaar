#lang racket/base
;;; Copyright (C) Laurent Orseau, 2010-2013
;;; GNU Lesser General Public Licence (http://www.gnu.org/licenses/lgpl.html)

(provide (all-defined-out))

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
