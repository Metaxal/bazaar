#lang racket/base

(require define2
         (for-syntax racket/syntax
                     syntax/parse
                     racket/base))

(provide struct+
         with-struct
         define-with-struct
         with-struct.id
         define-with-struct.id
         call
         call/apply)

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
                     [updater (format-id #'struct-id "update-~a-~a!" struct-sym field-sym)]
                     [updater+ (format-id #'struct-id "~a-~a!!" struct-sym field-sym)])
       #'(begin
           (define setter+ setter)
           (define (updater stru f)
             (setter stru (f (getter stru))))
           (define updater+ updater))))]))

;; Like `struct` but with more options
;; - If the #:mutable+ keyword is found, it is replaced with #:mutable,
;; and accessors and mutators are added. See the tests for examples.
;; (Does not yet handle parent arguments properly, nor per-field mutation.)
;; - The #:make/kw creates a `id`/kw constructor that takes keyword arguments.
;; http://www.greghendershott.com/2015/07/keyword-structs-revisited.html
;; Warning: For now, the optional arguments syntax is supported only with #:make/kw
;; TODO: handle parent struct and parent fields! (especially for constructor)
;; (idea: ask for the parent's make/kw constructor and use procedure-keywords.
;; How to do that at expansion time?)
(define-syntax (struct+ stx)
  (define-syntax-class field
    (pattern id:id
             #:with ctor-arg #`(#,(syntax->keyword #'id) id))
    (pattern [id:id default:expr]
             #:with ctor-arg #`(#,(syntax->keyword #'id) [id default])))
  (syntax-parse stx
    [(_ struct-id:id (field:field ...)
        (~or (~optional (~and (~seq #:mutable+) (~seq mut+)))
             (~optional (~and (~seq #:make/kw) (~seq make/kw)))
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
                     ;; TODO: USE PROCEDURE
                     #'()))]))

; Creates a function that works like plop/kw, except that I need to figure out how
; to ensure that arguments are fed in correct order.
#;
(define plop2/kw
    (let-values ([(mand opt) (procedure-keywords plop/kw)])
      (procedure-reduce-keyword-arity
       (make-keyword-procedure
        (λ(kws kw-args . rest)
          (list kws kw-args rest)))
       0
       mand
       opt)))

#;(define (keywords proc)
  (let-values ([(mand opt) (procedure-keywords proc)])
    (list mand (remove* mand opt))))

(module+ test
  (struct+ plop (x [y 2] z) #:mutable+ #:make/kw #:transparent)
  
  (let ([p (plop 10 20 30)])
    (check-eqv? (plop-x p) 10)
    (check-eqv? (plop-y p) 20)
    (check-eqv? (plop-z p) 30)
    (plop-x! p 5)
    (check-eqv? (plop-x p) 5)
    (plop-y!! p sub1)
    (update-plop-y! p sub1)
    (check-eqv? (plop-y p) 18))
  
  (let ([p (plop/kw #:x 3 #:z 4)])
    (check-equal? (list (plop-x p) (plop-y p) (plop-z p))
                  '(3 2 4)))
  (let ([p (plop/kw #:z 4 #:y 6 #:x 3)])
    (check-equal? (list (plop-x p) (plop-y p) (plop-z p))
                  '(3 6 4)))
  
  )

(begin-for-syntax
  (define-syntax-class id/maybe-rename
    (pattern old-id:id
             ;#:with old-id #'old-id
             #:with new-id #'old-id)
    (pattern [old-id:id new-id:id]
             ;#:with old-id #'old-id
             ;#:with new-id #'new-id*
             )))

;; Do a with-struct with let-like wrapping also?
;; Or better, write struct-obj.val
(define-syntax (with-struct stx)
  (syntax-parse stx
    [(_ struct-obj:expr struct-id:id (field:id/maybe-rename ...) body ...)
     (with-syntax* ([(get-id ...) (map (λ (fid) (format-id #'struct-id "~a-~a" #'struct-id fid))
                                       (syntax->list #'(field.old-id ...)))]
                    [(set-id! ...) (map (λ (fid) (format-id #'struct-id "set-~a-~a!" #'struct-id fid))
                                       (syntax->list #'(field.old-id ...)))])
       #'(let ([obj struct-obj])
           (let-syntax ([field.new-id
                         (make-set!-transformer
                          (λ (stx)
                            (syntax-case stx (set!)
                              [(set! id v) #'(set-id! obj v)]
                              [id (identifier? #'id) #'(get-id obj)])))]
                        ...)
             body ...)))]))

(define-syntax (define-with-struct stx)
  (syntax-parse stx
    [(_ struct-obj:id struct-id:id (field:id/maybe-rename ...))
     (with-syntax* ([(get-id ...) (map (λ (fid) (format-id #'struct-id "~a-~a" #'struct-id fid))
                                       (syntax->list #'(field.old-id ...)))]
                    [(set-id! ...) (map (λ (fid) (format-id #'struct-id "set-~a-~a!" #'struct-id fid))
                                       (syntax->list #'(field.old-id ...)))])
       #'(begin (define-syntax field.new-id
                  (make-set!-transformer
                   (λ (stx)
                     (syntax-case stx (set!)
                       [(set! id v) #'(set-id! struct-obj v)]
                       [id (identifier? #'id) #'(get-id struct-obj)]))))
                 ...))]))

(define-syntax (with-struct.id stx)
  (syntax-parse stx
    [(_ struct-obj:id struct-id:id (field:id ...) body ...)
     (with-syntax* ([(get-id ...) (map (λ (fid) (format-id #'struct-id "~a-~a" #'struct-id fid))
                                       (syntax->list #'(field ...)))]
                    [(set-id! ...) (map (λ (fid) (format-id #'struct-id "set-~a-~a!" #'struct-id fid))
                                        (syntax->list #'(field ...)))]
                    [(obj.id ...) (map (λ (fid) (format-id #'struct-id "~a.~a" #'struct-obj fid))
                                       (syntax->list #'(field ...)))])
       #'(let-syntax ([obj.id
                       (make-set!-transformer
                        (λ (stx)
                          (syntax-case stx (set!)
                            [(set! id v) #'(set-id! struct-obj v)]
                            [id (identifier? #'id) #'(get-id struct-obj)])))]
                      ...)
           body ...))]))

(define-syntax (define-with-struct.id stx)
  (syntax-parse stx
    [(_ struct-obj:id struct-id:id (field:id ...))
     (with-syntax* ([(get-id ...) (map (λ (fid) (format-id #'struct-id "~a-~a" #'struct-id fid))
                                       (syntax->list #'(field ...)))]
                    [(set-id! ...) (map (λ (fid) (format-id #'struct-id "set-~a-~a!" #'struct-id fid))
                                       (syntax->list #'(field ...)))]
                    [(obj.id ...) (map (λ (fid) (format-id #'struct-id "~a.~a" #'struct-obj fid))
                                       (syntax->list #'(field ...)))])
       #'(begin (define-syntax obj.id
                  (make-set!-transformer
                   (λ (stx)
                     (syntax-case stx (set!)
                       [(set! id v) #'(set-id! struct-obj v)]
                       [id (identifier? #'id) #'(get-id struct-obj)]))))
                 ...))]))


(module+ test
  (struct A (a b c) #:mutable #:transparent)
  
  (define A1 (A 1 2 3))
  
  (check-equal?
   (with-struct A1 A (a b [c d])
                (list b d a))
   '(2 3 1))
  (with-struct A1 A (a b [c d])
    (set! a 5)
    (set! d 6)
    (check-equal? (list a b d)
                  '(5 2 6)))
  
  
  (check-equal?
   (with-struct.id A1 A (a b c)
                  (list A1.b A1.c A1.a))
   '(2 6 5))

  (let ()
    (define A1 (A 1 2 3))
    (define-with-struct.id A1 A (a b c))
    (set! A1.a 12)
    (check-equal? (list A1.a A1.b (A-c A1))
                  '(12 2 3))))

;; For struct objects, saves a name and a pair of parenthesis (for clarity)
;; Assumes the first argument to method is the object itself
(define-syntax-rule (call method obj args ...)
  ((method obj) obj args ...))

;; Same as `call` but the last argument must be a list, to which the method is applied
(define-syntax-rule (call/apply method obj args ... rargs)
  (apply (method obj) obj args ... rargs))

(module+ test
  (let ()
    (struct A (x proc))
    (define a1 (A 3 (λ(this v1 v2)(list (A-x this) v1 v2))))
    (check-equal? (call A-proc a1 4 5) (list 3 4 5))
    (check-equal? (call/apply A-proc a1 '(5 6)) '(3 5 6))
    ))

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

