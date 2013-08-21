#lang racket/base
;;; Copyright (C) Laurent Orseau, 2010-2013
;;; GNU Lesser General Public Licence (http://www.gnu.org/licenses/lgpl.html)

(provide (all-defined-out))

;; Return the multiple values of proc-call as a list
(define-syntax-rule (values->list expr)
  (call-with-values (λ()expr) (λ l l)))
; Example:
; (values->list (values 1 2 3))
; -> '(1 2 3)
