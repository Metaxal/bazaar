#lang racket/base
(require syntax/parse/define
         (for-syntax racket/base))

(provide begin/cond
         cond/else)

;; Reduce rightward drift (and parentheses) when chaining conds
;; interleaved with defines and other 

(define-syntax (begin/cond stx)
  (syntax-parse stx
    [(_ body:expr ...
        (~seq #:cond [test test-body ...] ...)
        (~seq #:else else-body ...))
     #'(begin
       body ...
       (cond [test test-body ...]
             ...
             [else
              (begin/cond else-body ...)]))]
    [(_ body:expr ...)
     #'(begin body ...)]))

;; Like begin/cond, but starting with #:cond
(define-simple-macro (cond/else body1:expr body2 ...)
  (begin/cond #:cond body1 body2 ...))

;; Example.
(module+ drracket
  ;; racket/base form.
  (let ()
    (displayln "start")
    (define y 2)
    (cond
      [(> y 3) 'y>3]
      [(not 5) 'well_nope]
      [else
       (displayln "do")
       (define x (+ y 2))
       (cond
         [(> x 3) 'x>3]
         [else
          (define z (+ x y))
          (cond
            [(< z 10) 'z<10]
            [else 'other])])]))

  ;; With begin/cond.
  (begin/cond
    (displayln "start")
    (define y 2)
    #:cond
    [(> y 3) 'y>3]
    [(not 5) 'well_nope]
    #:else
    (displayln "do")
    (define x (+ y 2))
    #:cond
    [(> x 3) 'x>3]
    #:else
    (define z (+ x y))
    #:cond
    [(< z 10) => values]
    #:else
    'other))
