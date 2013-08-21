#lang racket/base
;;; Copyright (C) Laurent Orseau, 2010-2013
;;; GNU Lesser General Public Licence (http://www.gnu.org/licenses/lgpl.html)

(require (only-in racket/gui/base message-box))

(provide (all-defined-out))

;; Exception handler for exn:mesg-error
(define (msg-error-box e)
  (message-box "Error" (exn-message e)
               #f '(ok stop)))

;; Syntactic salt for msg-error
(define-syntax-rule (with-msg-box-error-handler body ...)
  (with-handlers ([exn:fail? msg-error-box])
    body ...
    ))
  
;; Examples
(module+ main
  (with-msg-box-error-handler
   (printf "before\n")
   (error "inside")
   (printf "after")
   )
  )