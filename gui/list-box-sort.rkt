#lang racket/base
;;; Copyright (C) Laurent Orseau, 2010-2013
;;; GNU Lesser General Public Licence (http://www.gnu.org/licenses/lgpl.html)

(require "../list.rkt"
         racket/gui/base
         racket/class
         racket/function
         racket/format
         racket/list
         )

(provide list-box-sort%)

#|
WARNING: Not memory-efficient because the contents are both in Racket's list-box% and here.
But Racket's list-box is such a mess for multi-column handling that I didn't see an easy
way around: list-box% has no `get' method, and no `append-list', the initial `choices' is 
useless, etc.

TODO:
- Keep the current selection active, when sorting


comparators: (list-of comparator)
comparator: (or (λ(val1 val2) -> boolean?) 
                (λ(val1 data1 val2 data2) -> boolean?))
|#
(define list-box-sort%
  (class list-box%
    (init-field columns
                [comparators #f]
                [min-widths (build-list (length columns) (λ(n) #f))]
                [rows '()]
                )
    (init [min-height 0]
          [style '()]
          [[inner-callback callback] void])
    
    (unless comparators
      (set! comparators (build-list (length columns) (λ(n) (λ _ #t)))))
    (when (not (= (length comparators) (length columns)))
      (error "labels and comparators must have the same length"))
    
    (when (not (= (length min-widths) (length columns)))
      (error "labels and min-width must have the same length"))
    
    (super-new [min-height min-height]
               [columns columns]
               [callback (λ(lb ev)
                           (if (eq? (send ev get-event-type) 'list-box-column)
                               (this-sort (send ev get-column))
                               (inner-callback lb ev)))]
               [choices '()]
               [style (append style '(column-headers clickable-headers))]
               )
    
    (define lb-values ; (list row data)
      ; use the first list-value as the data for each row
      (map (λ(l)(list l (first l))) rows))
    
    (define last-sorted #f)
    
    (define (update)
      ; Not very efficient...
      (if (empty? lb-values)
          (clear)
          (send/apply this set (transpose (map first lb-values)))))
    
    ;; rows: list of rows. Elements of the rows are turned into strings.
    ;; ldata: list of data, one per row
    (define/public (set-choices rows [ldata rows])
      (set! lb-values (map (λ(r d)(list (map ~a r) d)) rows ldata))
      (update))

    (define/override (clear)
      (set! lb-values '())
      (super clear))
    
    ;; Internal name: this-append
    (override [this-append append])
    (define (this-append lstr [data lstr])
      (set! lb-values (consr (list lstr data) lb-values))
      (update))
    
    (define/public (get-selection-data)
      (second (list-ref lb-values (send this get-selection))))
    
    (public [this-sort sort])
    (define (this-sort num-column)
      (let* ([comparator (list-ref comparators num-column)]
             [comparator (if (procedure-arity-includes? comparator 2)
                             ; takes only two elements, transform to take 4:
                             (λ(val-data1 val-data2)(comparator (first val-data1) (first val-data2)))
                             ; else, supposed to already take 4 arguments:
                             (λ(val-data1 val-data2)(apply comparator (append val-data1 val-data2)))
                             )]
             [comparator (if (equal? last-sorted num-column) (negate comparator) comparator)]
             [lb-vals (sort lb-values
                            comparator
                            #:key (λ(l)(list (list-ref (first l) num-column) (second l)))
                            )])
        (set! lb-values lb-vals)
        (update)
        (set! last-sorted (if last-sorted #f num-column))
        ))
        
    (update) ; to initialize the choices correctly
    ))


;;; TESTS:
(module+ main
  (define frame (new frame% [label ""] [min-height 400] [min-width 400]))
  
  (define (make-comp n)
    (λ(val1 data1 val2 data2)
      (<= (list-ref data1 n) (list-ref data2 n))))
  
  (define lbc (new list-box-sort%
                   [parent frame]
                   [label "Sorted columns"]
                   [columns '("a" "b" "c")]
                   [comparators 
                    ;(list string<? string<? string<?)]
                    (build-list 3 make-comp)]
                   ;#f]
                   [min-widths '(80 90 100)]
                   [min-height 00]
                   [style '(single vertical-label)]
                   [callback (λ(lbc evt)
                               (displayln (send lbc get-selection-data)))]
                   ))
  
  (define lbc2 (new list-box% 
                    [parent frame]
                    [label "List-box"]
                    [choices '()]
                    [columns '("a" "b" "c")]
                    [style '(single vertical-label column-headers clickable-headers)]
                    [callback (λ(lb ev)
                                (case (send ev get-event-type)
                                  [(list-box-column) (displayln (send ev get-column))])
                                (displayln (send lbc2 get-selection)))]))
  
  (define ll
    (for/list ([i 30])
      (build-list 3 (λ(n)(random 100)))))
  (send lbc set-choices ll)
  
  (send/apply lbc2 set (map (λ(l)(map number->string l)) (transpose ll)))
  (send lbc2 append "coucou" "plop")
  (send lbc2 append "yop!")
  
  (send frame show #t)
  
  )