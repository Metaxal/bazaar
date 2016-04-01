#lang racket/base

(require racket/dict
         racket/list
         racket/provide
         (for-syntax racket/base))

(provide dict-list-filter
         dict-list-distinct-values
         dict-list-values
         dict-list-count
         dict-list-group-by
         dict-list-count-by)

;::::::::::::::::::::::::::::::;
;:: Dict-List data structure ::;
;::::::::::::::::::::::::::::::;

;;;; A dict-list (dlist) is a list of similar dictionaries.
;;;; For example, this can be used as a data points with various dimensions.
;;;; Such a structure can easily be saved to a file
;;;; and then reloaded with file->value (if the list of data points is stored)
;;;; or file->list (which is useful when data points are saved online in the file
;;;; without begin/end codes).
;;;; The dictionaries may not be sorted but the order of the data points
;;;; is preserved by the following operations unless specified otherwise.

(define (proc-and-arity? proc n)
  (and (procedure? proc)
       (procedure-arity-includes? proc n)))

(define (key-or-proc->dict-key-ref key-or-proc)
  (if (proc-and-arity? key-or-proc 1)
      key-or-proc
      (λ(d)(dict-ref d key-or-proc))))

;; keeps only the listed keys  for each data point
(define (dict-list-filter-keys dlist keys)
  (for/list ([d (in-list dlist)])
    (filter (λ(p)(member (car p) keys)) d)))

;; Returns the list of data points that match the requirements in the key+value-dict.
;; The value of the dictionary can either be a value or a arity-1 procedure that will
;; be applied to the value of the data point.
(define (dict-list-filter dlist . key+value-dict)
  (filter (λ(d) (for/and ([(k v) (in-dict key+value-dict)])
                  (if (proc-and-arity? v 1)
                      (v (dict-ref d k))
                      (equal? v (dict-ref d k)))))
          dlist))

;; Returns all the values for a given key, in the same order.
(define (dict-list-values dlist key)
  (map (λ(d)(dict-ref d key)) dlist))

;; Returns all the distinct values for the given key in the data list.
(define (dict-list-distinct-values dlist key)
  (remove-duplicates (dict-list-values dlist key)))

;; Groups the data based on key-or-proc
(define (dict-list-group-by dlist key-or-proc)
  (define dict-key-ref (key-or-proc->dict-key-ref key-or-proc))
  (group-by dict-key-ref dlist))

;; Returns the number of occurences for each possible value of key-or-proc
(define (dict-list-count-by dlist key-or-proc)
  (define dict-key-ref (key-or-proc->dict-key-ref key-or-proc))
  (define group-list
    (group-by dict-key-ref dlist))
  (for/list ([one-group (in-list group-list)])
    (cons (dict-key-ref (first one-group)) (length one-group))))

(define (dict-list-count dlist . key+value-dict)
  (count (λ(d) (for/and ([(k v) (in-dict key+value-dict)])
                 (if (proc-and-arity? v 1)
                     (v (dict-ref d k))
                     (equal? v (dict-ref d k)))))
         dlist))

;; Groups the key-value pairs base based on the keys (kind of transposed of the data table)
(define (dict-list-group-pairs-by dlist)
  (group-by car (apply append dlist)))

(module+ test
  (require rackunit
           racket/set)
  (define dic
    '(((a . 1) (b . b))
      ((a . 2) (b . b))
      ((a . 1) (b . b))
      ((a . 3) (b . b))
      ((a . 2) (b . b))))
  

  (check set=? (dict-list-count-by dic 'a)
                '((1 . 2) (2 . 2) (3 . 1)))
  )

;;; See usage example at the end of "restarting-kt.rkt"
