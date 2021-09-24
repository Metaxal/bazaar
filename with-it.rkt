#lang racket/base
(require (for-syntax racket/base
                     syntax/parse))

(provide with)

(define-syntax (with stx)
  (syntax-parse stx
    [(with it last)
     #'last]
    [(with it body1 body2 ...)
     #'(let ([it body1])
         (with it body2 ...))]))

;;; Example
(module+ drracket
  (require racket)
  (with it
        "Hey\nThis is a nice little doggy you have here\nCome here doggy!\nNice doggy indeed\nSee ya!"
        (string-split it "\n")
        (filter (λ (line) (string-contains? line "doggy")) it)
        (sort it string<?)
        (remove-duplicates it)))