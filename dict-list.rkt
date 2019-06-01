#lang racket/base

(require racket/dict
         racket/list
         racket/provide
         (for-syntax racket/base))

(provide dict-list-filter
         dict-list-keys
         dict-list-distinct-values
         dict-list-values
         dict-list-count
         dict-list-group-by
         dict-list-count-by
         dict-list->table)

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

(define (dict-list-keys dlist)
  (remove-duplicates
   (for*/list ([dic (in-list dlist)]
               [(k v) (in-dict dic)])
     k)))

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

;; Turns a list of dictionaries into a table; the keys are inserted on the first line.
;; The keys are kept in order of appearence.
;; Possibly to be used in conjunction with text-table.
(define no-arg (gensym))
(define (dict-list->table dl #:absent [absent no-arg])
  (define keys (remove-duplicates (append* (map dict-keys dl))))
  (cons
   keys
   (for/list ([d (in-list dl)])
     (map (λ(k)(if (eq? absent no-arg)
                   (dict-ref d k)
                   (dict-ref d k absent)))
          keys))))

(module+ test
  (check-equal? (dict-list->table '(((a . 5) (b . "b") (c . c))
                                    ((a . 6) (b . "b2") (d . d2))
                                    ((e . eee)))
                                  #:absent #f)
                '((a b c d e)
                  (5 "b" c #f #f)
                  (6 "b2" #f d2 #f)
                  (#f #f #f #f eee)))
  (check-exn exn?
             (λ()(dict-list->table '(((a . 5) (b . "b") (c . c))
                                     ((a . 6) (b . "b2") (d . d2))
                                     ((e . eee)))))))


(module* stats #f
  (require math
           racket/file
           bazaar/dict
           bazaar/plot)

  (provide log-file->dict-list
           plot-profiles
           dict-list-stats)
  
  ;; A 'data' is a dict-list, i.e. a list of similar dictionaries
  
  (define (log-file->dict-list f)
    (filter list? (file->list f)))
  
  (define (stats l)
    (if (empty? l)
        (list (cons "### empty list ###" ""))
        (list (cons 'length (length l))
              (cons 'mean (mean l))
              (cons 'std-dev (sqrt (variance l)))
              (cons 'median (median < l))
              (cons 'sum (apply + l))
              (cons 'max (apply max l))
              (cons 'min (apply min l)))))
  
  (define (plot-data-l l)
    (if (empty? l)
        "Cannot plot empty list"
        (plot
         (list (points (list->points (sort l <)))
               (x-axis 0))
         #:x-label "Sorted data index" #:y-label "Redundancy")))
  
  (define (plot-profiles dlist keys)
    (plot
     (for/list ([k keys] [i (in-naturals)])
       (lines (list->points (sort (dict-list-values dlist k) <))
              #:label k #:color i))
     #:width 800 #:height 600))
  
  ;; key: (or/c any? procedure?) ;
  ;; If key is a procedure, then it is applied to a dictionary to retrieve a value.
  ;; If key2 is given, key1 must be a key and 
  (define (dict-list-stats dlist key [key2 #f] [cmp -])
    (define l
      (cond [key2 (map (λ(d)(- (dict-ref d key) (dict-ref d key2))) dlist)]
            [(procedure? key) (map key dlist)]
            [else (dict-list-values dlist key)]))
    (printf "key1: ~v\n" key)
    (when key2 (printf "key2: ~v\n" key2))
    (println (plot-data-l l))
    (assoc-nice-print
     (append (cons (cons "*** all ***" "")
                   (stats l))
             (cons (cons "*** negative ***" "")
                   (stats (filter negative? l)))
             (cons (cons "*** positive ***" "")
                   (stats (filter positive? l))))))
  )
;;; See usage example at the end of "restarting-kt.rkt"


