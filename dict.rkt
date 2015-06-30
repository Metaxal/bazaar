#lang racket/base
;;; Copyright (C) Laurent Orseau, 2010-2013
;;; GNU Lesser General Public Licence (http://www.gnu.org/licenses/lgpl.html)

(require (prefix-in dict: racket/dict))

(provide make-massoc-dict #;massoc
         occurrences)

(module+ test 
  (require rackunit))

(define (make-massoc-dict . pairs)
  (massoc pairs))

;; A mutable assoc-list dictionary
;; (keeps ordering of the elements)
;; Actually just a boxed immutable assoc-list
(struct massoc (v)
  #:transparent
  #:mutable
  #:methods dict:gen:dict
  [(define (dict-ref dict key
                     [default (lambda () (error "key not found" key))])
     (dict:dict-ref (massoc-v dict) key default))
   
   (define (dict-set dict key val)
     (massoc (dict:dict-set (massoc-v dict) key val)))
   
   (define (dict-set! dict key val)
     (set-massoc-v! dict (dict:dict-set (massoc-v dict) key val)))
   
   (define (dict-remove dict key)
     (massoc (dict:dict-remove (massoc-v  dict) key)))
   
   (define (dict-remove! dict key)
     (set-massoc-v! dict (dict:dict-remove (massoc-v dict) key)))
   
   (define (dict-count dict #:default [x #f])
     (dict:dict-count (massoc-v dict) #:default x))
   
   (define (dict-iterate-first dict)
     (dict:dict-iterate-first (massoc-v dict)))
   
   (define (dict-iterate-next dict pos)
     (dict:dict-iterate-next (massoc-v dict) pos))
   
   (define (dict-iterate-key dict pos)
     (dict:dict-iterate-key (massoc-v dict) pos))
   
   (define (dict-iterate-value dict pos)
     (dict:dict-iterate-value (massoc-v dict) pos))
   ])

(module+ test 
  (define d1 (make-massoc-dict '(a . 1) '(b . 2)))
  
  (check-equal? (dict:dict-ref d1 'a) 1)
  (check-equal? (massoc-v (dict:dict-set d1 'a 3))
                '((a . 3) (b . 2)))
  (dict:dict-set! d1 'c 4)
  (check-equal? (massoc-v d1)
                '((a . 1) (b . 2) (c . 4)))
  (dict:dict-remove! d1 'b)
  (check-equal? (massoc-v d1)
                '((a . 1) (c . 4)))
  
  (check-equal? (for/list ([(k v) (dict:in-dict d1)])
                  (list k v))
                '((a 1) (c 4)))
  )


;; Returns a dictionary of (e . n) where e is an element of l,
;; and n is the number of times it occurs in l.
;; The kind of dictionary can be controlled with the `make-dict' argument.
(define (occurrences l #:make-dict [make-dict make-hash])
  (define h (make-dict))
  (for-each (λ(x)(dict:dict-update! h x add1 0)) l)
  h)

(module+ test
  (check-equal? (occurrences '(a b a c c d a) #:make-dict make-massoc-dict)
                (massoc '((a . 3) (b . 1) (c . 2) (d . 1)))))
