#lang racket/base
;;; Copyright (C) Laurent Orseau, 2010-2013
;;; GNU Lesser General Public Licence (http://www.gnu.org/licenses/lgpl.html)

(require (prefix-in dict: racket/dict)
         racket/format
         racket/string)

(provide make-massoc
         massoc
         massoc->list
         dict-transpose
         occurrences
         assoc-nice-print
         assoc-nice-write
         assoc-nice-display
         get-key
         is-key)

(module+ test 
  (require rackunit))

(define (make-massoc . pairs)
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

;; Faster than dict->list
(define (massoc->list m)
  (massoc-v m))

(module+ test 
  (define d1 (make-massoc '(a . 1) '(b . 2)))
  
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

(define (dict-transpose d)
  (list (dict:dict-keys d)
        (dict:dict-values d)))


;; Returns a dictionary of (e . n) where e is an element of l,
;; and n is the number of times it occurs in l.
;; The kind of dictionary can be controlled with the optional argument.
;; See also `samples->hash` and `count-samples` from math/statistics.
(define (occurrences l [d (make-hash)])
  (if (dict:dict-mutable? d)
      (begin (for-each (λ(x)(dict:dict-update! d x add1 0)) l)
             d)
      (for/fold ([d d])
                ([x (in-list l)])
        (dict:dict-update d x add1 0))))

(module+ test
  (let ([l '(a b a c c d a)]
        [res '((a . 3) (b . 1) (c . 2) (d . 1))])
    (check-equal? (massoc->list (occurrences l (make-massoc)))
                  res)
  (check-equal? (occurrences l '())
                res)))

(define ((assoc-nice-output ~out) ass)
  (define w (apply max (map (λ(p)(string-length (~v (car p)))) ass)))
  (displayln 
   (string-join
    #:before-first "("
    (for/list ([p ass])
      (string-append
       "(" (~a (car p) #:min-width w #:pad-string " ")
       " . " (~out (cdr p)) ")"))
    "\n "
    #:after-last ")")))

(define assoc-nice-print   (assoc-nice-output ~v))
(define assoc-nice-write   (assoc-nice-output ~s)) ; This is probably the one you want
(define assoc-nice-display (assoc-nice-output ~a))


#;
(assoc-nice-print
 '((name . "Henry")
   (aphabet-size . 90)
   (mixture-log-prob . -17095.10259010989)
   (mixture-log2-prob . -24663.019730239615)
   (mixture-bytes . 3082.877466279952)
   (file-length . 11150)
   (bits-per-byte . 2.2119300206492927)
   (nb-models . 38413)
   ("a key string" . "a value string")
   (|a key symbol| . |a value symbol|)
   (|a quite long key symbol| . #t)
   (ctx-length-max . 10)))
; ->
#;
((name              . "Henry") ; display outputs Henry without quotes
 (aphabet-size      . 90)
 (mixture-log-prob  . -17095.10259010989)
 (mixture-log2-prob . -24663.019730239615)
 (mixture-bytes     . 3082.877466279952)
 (file-length       . 11150)
 (bits-per-byte     . 2.2119300206492927)
 (nb-models         . 38413)
 (ctx-length-max    . 10))

(define ((get-key key [default (λ()(error "key not found" key))]) d)
  (dict:dict-ref d key default))

(module+ test
  (check-equal?
   (map (get-key 'time)
        '( [(solved? . #t) (time . 12345)]
           [(solved? . #t) (time . 234)]
           [(solved? . #f) (time . 90000)]))
   '(12345 234 90000)))

#;
(define ((is-key key val [=? equal?] [default (λ()(error "key not found" key))]) d)
  (=? (dict:dict-ref d key default)
      val))

(define is-key
  (let ([proc (λ (key val =? default)
                (λ (d)
                  (=? (dict:dict-ref d key default)
                      val)))]
        [=? equal?]
        [default (λ () (error "key not found"))])
    (case-lambda
      [(key val)             (proc key val =? default)]
      [(key =? val)          (proc key val =? default)] ; so that order is easy to read
      [(key =? val default)  (proc key val =? default)])))

(module+ test
  (check-equal?
   (filter (is-key 'time < 1000)
           '( [(solved? . #t) (time . 12345)]
              [(solved? . #t) (time . 234)]
              [(solved? . #f) (time . 90000)]))
   '([(solved? . #t) (time . 234)])))
