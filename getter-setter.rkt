#lang racket/base

(require define2
         (for-syntax racket/syntax 
                     racket/base)
         "define.rkt"
         racket/class ; because of define/public
         )

(provide (all-defined-out))

;;; Creates getters and setters based on identifiers names.
;;; Can be used with `define-properties'.

;; From a identifier in a class, defines a (get-<id>) method
;; that returns the value of the <id>.
;; Several ids can be given.
;; See also the built-in get-field macro.
(define-syntax (define-getter stx)
  (syntax-case stx ()
    [(_ definer id)
     (with-syntax ([get-id (format-id #'id "get-~a" #'id)])
       #'(definer (get-id) id))]
    [(_ definer id1 id2 ...)
     #'(begin (define-getter definer id1)
              (define-getter definer id2 ...))]
    ))

;; Defines a setter (set-<id> val) that sets the <id> field to val.
;; Several ids can be given.
(define-syntax (define-setter stx)
  (syntax-case stx ()
    [(_ definer id)
     (with-syntax ([set-id (format-id #'id "set-~a" #'id)])
       #'(definer (set-id val) (set! id val))
       )]
    [(_ definer id1 id2 ...)
     #'(begin (define-setter definer id1)
              (define-setter definer id2 ...))]
    ))

;; Helper to define both a getter and a setter for given field ids.
(define-syntax-rule (define-getter/setter definer arg ...)
  (begin (define-getter definer arg ...)
         (define-setter definer arg ...)))


;; Now use the above makers to define
;; actual forms for classes (using define/public).
(define-syntax-rule (getter id ...)
  (define-getter define/public id ...))

(define-syntax-rule (setter id ...)
  (define-setter define/public id ...))

(define-syntax-rule (getter/setter id ...)
  (define-getter/setter define/public id ...))

;(define-syntax-rule (define/setter ....


;;; Ditto for module-wise definitions/attributes (not class attributes)

; No need for the getter. Provide the identifier instead.
;(define-syntax-rule (m-getter id ...)
;  (define-getter define id ...))

(define-syntax-rule (m-setter id ...)
  (define-setter define/provide id ...))

;(define-syntax-rule (m-getter/setter id ...)
;  (define-getter/setter define id ...))


(define-syntax-rule (define/m-setter id val)
  (begin (define id val)
         (m-setter id)))

;(define/m-setter x 3)

;; TODO: rackunit tests
#| | #

(define-syntax-rule (make-setter var)
  (begin
;  (begin-for-syntax
;    (set! set-id-maker #'(λ(id)(->symbol-append "set-" id "!"))))
  (define-setter define/provide (λ(id)(->symbol-append "set-" id "!")) var)
    ))

(define x 3)
(make-setter x)
;|#

#| TESTS | #
(define a%
  (class object% (super-new)
    (field [x 3]
           [y "plip"])
    
    (getter/setter x y)
    
    (set-x 4)

    ))

(define a (new a%))
(send a get-x)
(send a set-x 10)
(send a get-x)
(send a set-y "ploup")
(send a get-y)
(get-field y a)

;|#

#| Tests | #

(define x 3)
(m-setter x)
x
(set-x 4)
x

;|#

