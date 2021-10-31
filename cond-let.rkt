#lang racket/base
(require define2
         (for-syntax racket/base
                     syntax/parse))

(provide cond-let)

;; Related post: http://jeapostrophe.github.io/2013-11-12-condd-post.html

(define-syntax (cond-let stx)
  (syntax-parse stx
    #:literals (else)
    [(_)
     #'(error 'cond-let "Fell through without else clause")]
    [(_ [else . d])
     #'(let () . d)]
    [(_ #:def def . tail)
     #'(let ()
         def
         (cond-let . tail))]
    [(_ #:defs (def ...) . tail)
     #'(let ()
         def ...
         (cond-let . tail))]
    [(_ #:let ([var val] ...) . tail)
     #'(let ([var val] ...)
         (cond-let . tail))]
    [(_ #:let* ([var val] ...) . tail)
     #'(let* ([var val] ...)
         (cond-let . tail))]
    [(_ [test . b] . more)
     #'(if test
         (let () . b)
         (cond-let . more))]))


(module+ main
  (require racket/list)
  
  (for/list ([i (in-range 10)])
    (cond-let
     [(= i 0) 0]
     #:let* ([j (+ i 2)]
             [k (* j 2)])
     [(even? i) k]
     #:let ([l (+ k 1)])
     [(< i 5) l]
     #:def (define m (* l 10))
     [else (+ l j)])))
