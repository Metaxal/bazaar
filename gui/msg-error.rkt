#lang racket/base

(require (only-in racket/gui/base message-box))

(provide (all-defined-out))

;; Exception handler for exn:mesg-error
(define (msg-error-box e)
  (message-box "Error" (exn-message e)
               #f '(ok stop)))

;; Syntactic salt for msg-error
(define-syntax-rule (with-error-to-msg-box body ...)
  (with-handlers ([exn:fail? msg-error-box])
    body ...
    ))

(define-syntax-rule (with-error-to-error-port body ...)
  (with-handlers ([exn:fail? (λ(e)(displayln (exn-message e)
                                             (current-error-port)))])
    body ...))
  
;; Examples
(module+ main
  (with-error-to-msg-box
   (printf "before\n")
   (error "inside")
   (printf "after")
   )
  )