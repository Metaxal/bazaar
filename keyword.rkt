#lang racket/base

(require racket/dict
         racket/list)

(provide keyword-apply/dict
         keyword-apply/simple)

(module+ test
  (require rackunit))

;; proc : procedure? ; procedure to apply
;; kw-dict : dict? ; dictionary of keywords and values
;;     A key can be either a keyword or a symbol that is turned into a keyword.
;; largs : list? ; positional arguments
;; Returns the result of the application of proc to the positional arguments
;; and to the keyword arguments.
(define (keyword-apply/dict proc kw-dict largs)
  (define alist
    (sort
     (for/list ([(k v) (in-dict kw-dict)])
       (cons
        (cond [(keyword? k) k]
              [(symbol? k) (string->keyword (symbol->string k))]
              [else (error "Not a keyword or symbol:" k)])
        v))
     keyword<? #:key car))
  (keyword-apply proc (map car alist) (map cdr alist) largs))

(define (list->pos+kw l)
  (let loop ([l l] [pos-args '()] [kw-dict '()])
    (if (empty? l)
        (values (reverse pos-args) kw-dict)
        (let ([x (first l)]
              [l (rest l)])
          (if (keyword? x)
              (if (empty? l)
                  (error "Keyword must be followed by a value")
                  (let ([y (first l)]
                        [l (rest l)])
                    (loop l pos-args (cons (cons x y) kw-dict))))
              (loop l (cons x pos-args) kw-dict))))))

(define (keyword-apply/simple proc args)
  (let-values ([(pos kw) (list->pos+kw args)])
    (keyword-apply/dict proc kw pos)))

(module+ test
  
  (define (f a b #:c c #:d [d 10])
    (list a b c d))
  
  (check-equal?
   (keyword-apply/dict f '((#:d . 4) (c . 3)) '(1 2))
   '(1 2 3 4))
  
  (check-equal?
   (keyword-apply/dict f (hash '#:d 4 'c 3) '(1 2))
   '(1 2 3 4))
  
  (let ([kw-dict '((d . 4))]
        [largs '(2)])
    (check-equal?
     (keyword-apply/dict f
                         (list* '(c . 3) kw-dict)
                         (list* 1 largs))
     '(1 2 3 4)))
  
  (check-equal?
   (keyword-apply/simple f '(1 2 #:d 4 #:c 3))
   '(1 2 3 4))
  )


;; TODO: Pass-through function that takes a function f, a set of arguments and keyword arguments
;; and returns a specialized version of f for these arguments

