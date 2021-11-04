#lang racket/base

(require syntax/parse/define)

(provide convert let/convert)

(define-syntax-parse-rule (convert x [predicate converter] ...)
  (cond
    [(predicate x) (converter x)] ...
    [else (error (format "Cannot convert ~a. Supported predicates: ~a"
                         'x
                         '(predicate ...)))]))

;; TODO: Export to bazaar?
;; Converts the x using the converter of the first matching predicate.
;; The results shadow the original x.
(define-syntax-parse-rule (let/convert ([x [predicate converter] ...]
                                        ...)
                            body ...)
  (let ([x (convert x [predicate converter] ...)]
        ...)
    body ...))

(module+ drracket
  ; Example
  (define lst 3 #;#(a b c))
  (let/convert ([lst [list? values]
                     [vector? vector->list]])
    (displayln lst)))