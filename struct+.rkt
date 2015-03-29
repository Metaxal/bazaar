#lang racket

(require (for-syntax racket/syntax) syntax/parse/define)

(provide struct+)

(module+ test
  (require rackunit))

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

;; Like `struct`, but if the #:mutable+ keyword is found, it is replaced with #:mutable,
;; and accessors and mutators are added.
;; See the tests for examples.
(define-simple-macro (struct+ id (field ...) (~optional (~seq #:mutable+)) . rest)
  (begin (struct id (field ...) #:mutable . rest)
         (begin (struct-field-mutable+ id field) ...)))

(module+ test
  (struct+ plop (x y) #:mutable+ #:transparent)
  (define p1 (plop 10 20))
  (check-eqv? (plop-x p1) 10)
  (check-eqv? (plop-y p1) 20)
  (plop-x! p1 5)
  (check-eqv? (plop-x p1) 5)
  (plop-y!! p1 sub1)
  (check-eqv? (plop-y p1) 19)
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