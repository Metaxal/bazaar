#lang racket/base

;;; Copyright (C) Laurent Orseau, 2010
;;; GNU General Public Licence 3 (http://www.gnu.org/licenses/)

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