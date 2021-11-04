#lang racket/base

(require syntax/parse/define
         (for-syntax racket/base
                     syntax/parse
                     racket/syntax-srcloc))

(provide convert let/convert)

(struct exn:fail:convert exn:fail (a-srcloc)
  #:property prop:exn:srclocs
  (λ (a-struct)
    (list (exn:fail:convert-a-srcloc a-struct))))

;; todo: syntax-loc #'x
(define-syntax (convert stx)
  (syntax-parse stx
    [(convert x:expr [predicate converter] ...)
     #`(let ([y x]) ; in case x is an expression
         (cond
           [(predicate y) (converter y)] ...
           [else
            (raise (exn:fail:convert
                    (format "Cannot convert `~a`\n Supported predicates: ~v\n Given: ~v"
                            'x
                            '(predicate ...)
                            y)
                    (current-continuation-marks)
                    #,(syntax-srcloc #'x)))]))]))

;; Converts the x using the converter of the first matching predicate.
;; The results shadow the original x.
(define-syntax-parse-rule (let/convert ([x:id [predicate converter] ...]
                                        ...)
                            body ...)
  (let ([x (convert x [predicate converter] ...)]
        ...)
    body ...))

(module+ drracket
  ; Example
  ; We could use just `define` instead of `define-syntax-rule`,
  ; but the latter is able to propagate the source-location information to
  ; convert, so that the error is displayed at the call site of convexrt-x,
  ; rather than at the call site of convert inside the function.
  (define-syntax-rule (convert-x lst)
    (convert lst
             [list? values]
             [vector? vector->list]
             [string? string->list]))
  (convert-x '(a b c))
  (convert-x #(a b c))
  (convert-x "abc")
  (convert-x 'abc) ; DrRacket will display the error here
  
  (define lst #(a b c))
  (let/convert ([lst [list? values]
                     [vector? vector->list]])
    (displayln lst)))