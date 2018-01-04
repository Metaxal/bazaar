#lang racket/base
;;; Copyright (C) Laurent Orseau, 2010-2013
;;; GNU Lesser General Public Licence (http://www.gnu.org/licenses/lgpl.html)

(require (for-syntax syntax/parse
                     racket/base)
         racket/stxparam
         "base.rkt"
         )

(provide debug-var
         debug-vars
         debug-vars/line
         debug-vars/loc
         debug-expr
         assert
         check-≃
         (rename-out [check-≃ check-approx=])
         check-sum=1
         check-proba-list
         vars->assoc
         info-str
         ok
         time*
         time*/seq)

;;; See also Racket's log facility:
;;; http://docs.racket-lang.org/reference/logging.html

(module+ test
  (require rackunit))

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

(define-syntax assert
  (syntax-parser
    [(_ expr:expr)
     #'(unless expr
         (error "Assertion failed:" 'expr))]
    [(_ expr:expr ctx:expr ...)
     #'(unless expr
         (error "Assertion failed:" 'expr 'with-context: (vars->assoc ctx ...)))]))

(module+ test
  (check-not-exn (λ()(assert (= 1 1))))
  (let ([x 3])
    (check-exn exn:fail? (λ()(assert (= x 2) x)))))

(define current-check-precision (make-parameter  1e-7))

;; Todo: rename to assert?
;; Checks that x is with ε of y, produces an error otherwise
(define-syntax check-≃
  (syntax-rules ()
    [(_ x y)(check-≃ x y (current-check-precision))]
    [(_ x y ε)
     (let ([xx x]
           [yy y]
           [εε ε])
       (unless (<= (abs (- xx yy)) εε)
         (error "check-≃ failed:" xx yy εε)))]))

(module+ test
  (check-≃ 0.99 1.009 0.02))

;; Checks that the elements of the list l sums to 1 within ε (1e-7 by default)
(define-syntax check-sum=1
  (syntax-rules ()
    [(_ l) (check-sum=1 l (current-check-precision))]
    [(_ l ε) (check-≃ (for/sum ([x l]) x) 1. ε)]))

(module+ test
  (check-sum=1 '(0.2 0.3 .5))
  (check-sum=1 #(0.2 0.3 .5)))

(define-syntax check-proba-list
  (syntax-rules ()
    [(_ l) (check-proba-list l (current-check-precision))]
    [(_ l ε)
     (begin
       (check-sum=1 l ε)
       (for ([x l])
         (unless (<= 0. x (+ 1. ε)) ; can be slightly larger than 1.??
           (error "Expected value in [0, 1], got" x))))]))

(module+ test
  (check-proba-list '(0.2 0.3 .5)))


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
