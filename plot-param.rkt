#lang racket
(require racket/gui
         plot)

(provide (all-from-out plot)
         plot/param
         make-plot-param)

(struct plot-param (name min max value precision)
  #:transparent
  #:mutable)

;; The keyword arguments are convenience just the same as the positional arguments
(define (make-plot-param name
                         [min 0] [max 1]
                         [value min]
                         [precision 100])
  (plot-param name min max value precision))

;; Todo:
;; - instead of a precision, take an ordered list of values,
;; so that we can have log-scales, special values, etc.
;; - automatic parameters for x-min, x-max, and y-min, y-max (no slider?)
;; - ability to remove snapshots.


(define (slider->param-value sl p)
  (define v (send sl get-value))
  (define m (plot-param-min p))
  (define M (plot-param-max p))
  (define precision (plot-param-precision p))
  (+ m
     (* (/ v precision)
        (- M m))))

(define (plot/param params renderer-tree-proc
                    #:x-min [x-min #f]
                    #:x-max [x-max #f]
                    #:y-min [y-min #f]
                    #:y-max [y-max #f]
                    #:width [width (plot-width)]
                    #:height [height (plot-height)]
                    #:title [title (plot-title)]
                    #:x-label [x-label (plot-x-label)]
                    #:y-label [y-label (plot-y-label)]
                    #:legend-anchor [legend-anchor (plot-legend-anchor)]
                    )
  (define param-snapshots '())
  (define (paint-cv cv dc)
    (define-values (w h) (send cv get-client-size))
    (plot/dc
     (for/list ([param-vals (in-list (cons (map plot-param-value params)
                                           param-snapshots))]
                [idx (in-naturals 0)])
       (apply renderer-tree-proc idx param-vals))
     dc
     0 0
     w h
     #:x-min x-min
     #:x-max x-max
     #:y-min y-min
     #:y-max y-max
     #:title title
     #:x-label x-label
     #:y-label y-label
     #:legend-anchor legend-anchor
     ))
  (define fr (new frame% [label "gui"]))
  (define cv (new canvas%
                  [parent fr]
                  [min-width width]
                  [min-height height]
                  [paint-callback paint-cv]))
  ;; Creates the sliders for each parameter
  (for ([p (in-list params)])
    (define precision (plot-param-precision p))
    (define hp (new horizontal-panel%
                    [parent fr]
                    [stretchable-height #f]))
    (define sl
      (new slider%
           [parent hp]
           [label (plot-param-name p)]
           [min-value 0]
           [max-value precision]
           [callback
            (λ(sl ev)
              (define v (slider->param-value sl p))
              (set-plot-param-value! p v)
              (send tf set-value (~a v))
              (send cv refresh-now))]))
    (define tf
      (new text-field%
           [parent hp]
           [label #f]
           [init-value (~a (plot-param-value p))]
           [stretchable-width #f]
           [min-width 100]
           [callback
            (λ(tf ev)
              (when (eq? (send ev get-event-type) 'text-field-enter)
                (define tf-val (send tf get-value))
                (define v (string->number tf-val))
                (if v
                    (begin (set-plot-param-value! p v)
                           (send cv refresh-now))
                    (error (format "Not a number ~a for plot-param ~a" tf-val (plot-param-name p))))))]))
    (void))
  (define bt-snapshots
    (new button%
         [parent fr]
         [label "Snapshot"]
         [callback (λ(bt ev)
                     (set! param-snapshots
                       (append param-snapshots
                               (list (map plot-param-value params))))
                     (send cv refresh-now))]))
  (send fr show #t))

;:::::::::::::;
;:: Example ::;
;:::::::::::::;

(module+ main #;drracket

  (struct example (name doc code thunk)
    #:transparent)

  (define examples '())
  (define-syntax-rule (make-example name doc-string code)
    (set! examples
      (cons (example name doc-string 'code (λ()code))
            examples)))
  (define-syntax-rule (make-simple-examples code ...)
    (begin (make-example (+ 1 (length examples))
                         ""
                         code)
           ...))

  (make-simple-examples
   
   (plot/param
    (list (make-plot-param "ε" -1 1 .5 100)
          (make-plot-param "x-max" 1 100 2 100))
    (λ(idx ε x-max)
      (list
       (function log 0 x-max #:color 0 #:label "ln")
       (function (λ(x)(/ (- 1 (expt x (- ε))) ε)) #:color (+ (* 2 idx) 2)
                 #:label (format "(1-x^{-ε})/ε, ε=~a" ε))
       (function (λ(x)(/ (- (expt x ε) 1) ε)) #:color (+ (* 2 idx) 3)
                 #:label (format "(x^ε-1)/ε, ε=~a" ε)))))

  (begin
    (define-values (α-param X-param)
      (values (make-plot-param "α" 0 1 .5 100)
              (make-plot-param "X" 1 1000 2 1000)))
    (plot/param
     (list α-param X-param)  
     (λ(idx α X) ; same order as the list of parameters, idx is the index of the current snapshot (0 if no snapshot)
       (list
        (function (λ(x)
                    (/ (log (+ 1 (* α (- x 1))))
                       α))
                  0 X)
        (function (λ(x)
                    (/ (- x 1)
                       (+ 1 (* α (- x 1))))) #:color "red")
        (function (λ(x)(log x)) #:color "blue")
        (function (λ(x)(- x 1)) #:color "darkgreen"))))
    (map plot-param-value (list α-param X-param))) ; then use this to get the values after plotting


  (plot/param
   (list (make-plot-param "α" 0 1 .5 100)
         (make-plot-param "β" 0 1 .5 100)
         (make-plot-param "X" 1 1000 2 1000))
   (λ(idx α β X)
     (list
      (function (λ(x)(log (+ 1 x))) #:color "black" #:label "ln1+x")
      #;(function (λ(x)(- (expt (+ 1 (* α x)) β)
                          (expt (+ 1 (* β x)) α)))
                  #:label "(1+αx)^β - (1+βx)^α"
                  0 X)
      (function (λ(x)(expt (+ 1 (* α x)) β))
                #:label "(1+αx)^β"
                0 X)
      (function (λ(x)(expt (+ 1 (* β x)) α))
                0 X
                #:label "(1+βx)^α"
                #:color "blue"))))

  (plot/param
   (list (make-plot-param "a" 0 10 .5 100)
         (make-plot-param "b" 0 10 2 100))
   (λ(idx a b)
     (list (function (λ(ε)(+ (expt a ε) (expt b ε)))
                     0 2 #:color "blue" #:label "a^ε + b^ε")
           (function (λ(ε)[expt (+ a b) ε])
                     0 2 #:label "(a+b)^ε")))
   #:x-label "δ")

  (plot/param
   (list (make-plot-param "ε" 0 1 .5 100)
         (make-plot-param "α" 0 1 .5 100)
         (make-plot-param "x-max" 1 10))
   (λ(idx ε α x-max)
     (list
      (function log 0 x-max #:color 0 #:label "ln")
      (function (λ(x)(/ (log (+ 1 (* α (- x 1)))) α)) #:color (+ (* 2 idx) 2)
                #:label (format "ln(1-α+αx)/α, α=~a" α))
      (function (λ(x)(/ (- (expt x ε) 1) ε)) #:color (+ (* 2 idx) 3)
                #:label (format "(x^ε-1)/ε, ε=~a" ε)))))

  
  )
  (make-example
   "logit"
   "Approximation of the logit function"
   (plot/param
    (list (make-plot-param "ε" 0 1 100))
    (λ(idx ε)(list 
              (function (λ(x)(- (* (/ 1 ε (expt 2 ε)) (- (/ (expt (- 1 x) ε)) (/ (expt x ε))))
                                (log (/ x (- 1 x))))) 0 1 #:color "cyan")
              (x-axis)))
    #:y-min -4 #:y-max 4))

  (require racket/cmdline)
  (command-line
   #:multi
   [("-l" "--list-example-names") "List the names of the examples"
                                  (for ([ex (in-list examples)])
                                    (displayln (example-name ex)))]
   [("-f" "--list-examples+doc") "List the names of the examples and their doc-strings"
                                 (for ([ex (in-list examples)])
                                   (displayln (example-name ex))
                                   (displayln (example-doc ex))
                                   (newline))]
   [("-x" "--example") ex-name
                       "Execute example named <ex-name>"
                       (for ([ex (in-list examples)])
                         (when (string=? ex-name (~a (example-name ex)))
                           (newline)
                           (displayln (example-name ex))
                           (displayln (example-doc ex))
                           (pretty-print (example-code ex))
                           ((example-thunk ex))))]
   ))