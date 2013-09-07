#lang racket/base
;;; Copyright (C) Laurent Orseau, 2010-2013
;;; GNU Lesser General Public Licence (http://www.gnu.org/licenses/lgpl.html)

(require (for-syntax racket/base))

(provide (all-defined-out))

;; Return the multiple values of proc-call as a list
(define-syntax-rule (values->list expr)
  (call-with-values (λ()expr) list))
; Example:
; (values->list (values 1 2 3))
; -> '(1 2 3)

; Idea by Jens Axel Søgaard
(define-syntax defv (make-rename-transformer #'define-values))
(define-syntax letv (make-rename-transformer #'let-values))
(define-syntax defm (make-rename-transformer #'match-define))

