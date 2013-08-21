#lang racket/base

;;; Copyright: Laurent Orseau 2010-2011
;;; Licence: LGPL

(require racket/gui/base
         racket/class
         racket/function
         racket/list
         (only-in framework panel:horizontal-dragable%))

(provide columns-box%
         )

#|

comparators: (list-of comparator)
comparator: (or (λ(val1 val2) -> boolean?) 
                (λ(val1 data1 val2 data2) -> boolean?))
|#
(define columns-box%
  (class panel:horizontal-dragable% ;horizontal-panel%
    (init-field labels
                [comparators #f]
                [callback void]
                [min-widths (build-list (length labels) (λ(n) #f))]
                )
    (init [min-height 0])
    
    (when (and comparators (not (= (length comparators) (length labels))))
      (error "labels and comparators must have the same length"))
    
    (super-new [min-height min-height])
    
    (define last-sorted #f)
    
    (define lboxes
      (for/list ([l (in-list labels)]
                 [w (in-list min-widths)]
                 [i (in-naturals)])
        (let ([vp (new vertical-panel% [parent this]
                       [min-width (or w 0)]
                       [min-height min-height]
                       [stretchable-width #t]
                       )])
          (when comparators
            (new button% [label l]
                 [parent vp]
                 [callback (λ _ (this-sort i))]
                 [font tiny-control-font]
                 [horiz-margin 0]
                 [stretchable-width #t]
                 ))
          (new list-box% [parent vp]
               [label (and (not comparators) l)]
               [choices '()]
               [style '(vertical-label single)]
               [font tiny-control-font]
               [horiz-margin 0]
               [stretchable-width #t]
               [callback (λ(lb evt)
                           (set-selection (send lb get-selection))
                           (let ([first-visible (send lb get-first-visible-item)])
                             (for-each (λ(lb)(send lb set-first-visible-item first-visible))
                                       lboxes))
                           (callback this evt)
                           )]
               ))))
    
    (define lb-values '())
    
    (define/public (get-number)
      (send (first lboxes) get-number))
    
    (define/public (set-selection n)
      (for-each (λ(lb)(send lb set-selection n))
                lboxes))
    
    (define/public (clear)
      (for-each (λ(lb)(send lb clear)) lboxes)
      (set! lb-values '())
      )
    
    ;; Internal name: this-append
    (public [this-append append])
    (define (this-append lstr [data (first lstr)])
      (for-each (λ(lb str)(send lb append str data))
               lboxes
               lstr
               )
      (set! lb-values (cons (list lstr data) lb-values))
      )
    
    (define/public (get-data n)
      (send (first lboxes) get-data n))
    (define/public (get-selection)
      (send (first lboxes) get-selection))
    (define/public (get-string-list-selection)
      (map (λ(lb)(send lb get-string-selection))
           lboxes))
    (define/public (get-selection-data)
      (get-data (get-selection)))
    
    (public [this-sort sort])
    (define (this-sort num-column)
      (let* ([comparator (list-ref comparators num-column)]
             [comparator (if (procedure-arity-includes? comparator 2)
                             ; takes only two elements, transform to take 4:
                             (λ(val-data1 val-data2)(comparator (first val-data1) (first val-data2)))
                             ; else, supposed to alraedy take 4 arguments:
                             (λ(val-data1 val-data2)(apply comparator (append val-data1 val-data2)))
                             )]
             [comparator (if (equal? last-sorted num-column) (negate comparator) comparator)]
             [lb-vals (sort lb-values
                            comparator
                            #:key (λ(l)(list (list-ref (first l) num-column) (second l)))
                            )])
        (clear)
        (for-each (λ(l)(this-append (first l) (second l))) lb-vals)
        (set! last-sorted (if last-sorted #f num-column))
        ))
        
    
    ))



;;; TESTS:
(module+ main
  (define frame (new frame% [label ""] [min-height 400]))
  
  (define (make-comp n)
    (λ(val1 data1 val2 data2)
      (<= (list-ref data1 n) (list-ref data2 n))))
  
  (define lbc (new columns-box%
                   [parent frame]
                   [labels '("a" "b" "c")]
                   [comparators 
                    ;(list string<? string<? string<?)]
                    (build-list 3 make-comp)]
                   ;#f]
                   [min-widths '(80 90 100)]
                   [min-height 00]
                   [callback (λ(lbc evt)
                               (displayln (send lbc get-selection-data)))]
                   ))
  
  (for ([i (in-range 30)])
    (let ([l (build-list 3 (λ(n)(random 100)))])
      (send lbc append (map number->string l) l)))
  
  (send frame show #t)
  )