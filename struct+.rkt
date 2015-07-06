#lang racket

(require (for-syntax racket/syntax
                     syntax/parse))

(provide struct+)

(module+ test
  (require rackunit))

(begin-for-syntax
 (define syntax->keyword (compose1 string->keyword symbol->string syntax->datum)))

(define-syntax (struct-field-mutable+ stx)
  (syntax-case stx ()
    [(_ struct-id field-id)
     (let ([struct-sym (syntax-e #'struct-id)]
           [field-sym (syntax-e #'field-id)])
       (with-syntax ([getter (format-id #'struct-id "~a-~a" struct-sym field-sym)]
                     [setter (format-id #'struct-id "set-~a-~a!" struct-sym field-sym)]
                     [setter+ (format-id #'struct-id "~a-~a!" struct-sym field-sym)]
                     [updater (format-id #'struct-id "~a-~a!!" struct-sym field-sym)])
       #'(begin
           (define setter+ setter)
           (define (updater stru f)
             (setter stru (f (getter stru)))))))]))

;; Like `struct` but with more options
;; - If the #:mutable+ keyword is found, it is replaced with #:mutable,
;; and accessors and mutators are added. See the tests for examples.
;; (Does not yet handle parent arguments properly, nor per-field mutation.)
;; - The #:make/kw creates a `id`/kw constructor that takes keyword arguments.
;; http://www.greghendershott.com/2015/07/keyword-structs-revisited.html
(define-syntax (struct+ stx)
  (define-syntax-class field
    (pattern id:id
             #:with ctor-arg #`(#,(syntax->keyword #'id) id))
    (pattern [id:id default:expr]
             #:with ctor-arg #`(#,(syntax->keyword #'id) [id default])))
  (syntax-parse stx
    [(_ struct-id:id (field:field ...)
        (~or (~optional (~and (~seq #:mutable+) (~seq mut+)))
             (~optional (~and (~or (~seq #:make/kw)) (~seq make/kw)))
             (~seq opt)) ...)
     #`(begin (struct struct-id (field.id ...) 
                #,@(if (attribute mut+) #'(#:mutable) #'())
                opt ...)
              #,@(if (attribute mut+)
                     #'((struct-field-mutable+ struct-id field.id) ...)
                     #'())
              #,@(if (attribute make/kw)
                     (with-syntax ([ctor-id (format-id #'struct-id "~a/kw" #'struct-id)]
                                   [((ctor-arg ...) ...) #'(field.ctor-arg ...)])
                       #'((define (ctor-id ctor-arg ... ...) ;i.e. append*
                            (struct-id field.id ...))))
                     #'()))]))

(module+ test
  (struct+ plop (x [y 2] z) #:mutable+ #:make/kw #:transparent)
  
  (let ([p (plop 10 20 30)])
    (check-eqv? (plop-x p) 10)
    (check-eqv? (plop-y p) 20)
    (check-eqv? (plop-z p) 30)
    (plop-x! p 5)
    (check-eqv? (plop-x p) 5)
    (plop-y!! p sub1)
    (check-eqv? (plop-y p) 19))
  
  (let ([p (plop/kw #:x 3 #:z 4)])
    (check-equal? (list (plop-x p) (plop-y p) (plop-z p))
                  '(3 2 4)))
  (let ([p (plop/kw #:z 4 #:y 6 #:x 3)])
    (check-equal? (list (plop-x p) (plop-y p) (plop-z p))
                  '(3 6 4)))
  )

;; Possibly interesting helper for updaters and others (but here is not the place)
#;
(begin
  (define (call1 f . l)
    (λ(x)(apply f x l)))
  
  (module+ test
    (define (foo x n b)
      (list (+ x n) b))
    
    (check-equal? (map (call1 foo 5 'a)
                       '(5 10 15 20))
                  
                  (map (λ(x)(foo x 5 'a))
                       '(5 10 15 20))))
  )

;;; A better/shorter solution would be to use the struct object as a getter/setter/updater:
;;; (struct plop (x y))
;;; (define p1 (plop 3 4))
;;; (p1 x) ; get x
;;; (p1 x 10) ; set x
;;; (p1 x v (+ v 10)) ; increment x by 10 (or use a more functional style)

