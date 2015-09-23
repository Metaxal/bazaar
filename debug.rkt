#lang racket/base
;;; Copyright (C) Laurent Orseau, 2010-2013
;;; GNU Lesser General Public Licence (http://www.gnu.org/licenses/lgpl.html)

(require (for-syntax syntax/parse
                     racket/base))

(provide debug-vars
         debug-vars/line
         debug-vars/loc
         debug-expr
         vars->assoc
         expr->expr+symbol
         info-str
         ok
         time*
         time*/seq)

;;; See also Racket's log facility:
;;; http://docs.racket-lang.org/reference/logging.html

(begin-for-syntax
  (define (syntax-source-path-string stx)
    (let* ([f (syntax-source stx)])
      (and (path-string? f) f))))

(define-syntax debug-var
  (syntax-parser 
   [(_ var:id)
    #'(printf "~a = ~v\n" 'var var)]))

(define-syntax debug-var/line
  (syntax-parser 
   [(_ var:id)
    (with-syntax ([line (syntax-line #'var)])
      #'(printf "~a: ~a = ~v\n" line 'var var))]))

(define-syntax debug-var/loc
  (syntax-parser 
   [(_ var:id)
    (with-syntax ([line (syntax-line #'var)]
                  [pth (syntax-source-path-string #'var)])
      #'(printf "~a:~a ~a = ~v\n" pth line 'var var))]))

(define-syntax-rule (debug-vars var ...)
  (begin (debug-var var) ...))

(define-syntax-rule (debug-vars/line var ...)
  (begin (debug-var/line var) ...))

(define-syntax-rule (debug-vars/loc var ...)
  (begin (debug-var/loc var) ...))

(define-syntax-rule (vars->assoc var ...)
  (list (cons 'var var) ...))

;; Returns the value expression and the quoted expression
(define-syntax-rule (expr->expr+symbol expr)
  (values expr 'expr))

;; Surround an expr with this procedure to output 
;; its value transparently
(define (debug-expr expr)
  (write expr)
  (newline)
  expr)

(define (info-str fmt . args)
  (display (string-append (apply format fmt args) "... ")))

(define (ok)
  (displayln "Ok."))

#| Example:
(info-str "Starting server")
<something to start the server>
(ok)
(info-str "Sarting client")
<something to start the client>
(ok)
|#

;; Like `time` but displays source location
(define-syntax (time* stx)
  (syntax-case stx ()
    [(_ body0 body1 ...)
     (with-syntax ([line (syntax-line #'body0)]
                   [pth (syntax-source-path-string #'body0)])
      #'(time (begin0 (begin body0 body1 ...)
                      (printf "at ~a line ~a:\n" pth line))))]))

(module+ test
  (time* 'bap))

;; Like `time*` but for each element of the body, wrapped in a begin
(define-syntax-rule (time*/seq body ...)
  (begin (time* body) ...))

(module+ test
  (time*/seq
   'bloop
   'blip
   'blap))
