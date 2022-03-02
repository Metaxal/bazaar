#lang racket/base

(require define2)

(provide (all-defined-out))

(define readtable-#<procedure:>
  (make-readtable
   (current-readtable)
   #\<
   'dispatch-macro
   (case-lambda
     [(ch port) ; read-mode only (no read-syntax)
     (define sym (read port))
     (define str (symbol->string sym))
     (vector (string->symbol (substring str 0 (- (string-length str) 1))))]
     [(ch port src line col pos)
      (error "not implemented")])
   ))

(module+ test
  (require rackunit
           racket/port)
  (check-equal?
   (parameterize ([current-readtable readtable-#<procedure:>])
     (with-input-from-string "(i guess #(is this #<procedure:argh> true?) it is)" read))
   '(i guess #(is this #(procedure:argh) true?) it is)))