#lang racket/base
;;; Copyright (C) Laurent Orseau, 2010-2013
;;; GNU Lesser General Public Licence (http://www.gnu.org/licenses/lgpl.html)

(provide (all-defined-out))

(define (sign x)
  (cond [(< x 0) -1]
        [(> x 0) 1]
        [else 0]))
