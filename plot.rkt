#lang racket/base

(require plot
         racket/list
         racket/dict)

(provide (all-defined-out)
         (all-from-out plot))

(module+ test
  (require rackunit))

;; Warning: Can have weird effects on DrRacket's window!
(define (string-ticks string-list)
  (let* ([N (length string-list)]
         [inf+sup->low+up
          (λ(inf sup)
            (when (> inf sup)
              (error 'bound-check "inf > sup in string-ticks; inf=~e, sup=~e" inf sup))
            (values (max 0 (inexact->exact (ceiling inf)))
                    (min (+ 1 (floor (inexact->exact sup))) N)))])
    (ticks (λ(inf sup)
             (define-values (low up) (inf+sup->low+up inf sup))
             (if (< low up)
                 (for/list ([i (in-range low up)])
                   (pre-tick i #t))
                 '()))
           (λ(inf sup pre-ticks)
             (for/list ([t pre-ticks])
               (define idx (pre-tick-value t))
               (if (and (exact-nonnegative-integer? idx)
                        (< idx N))
                   (list-ref string-list idx)
                   ""))))))
(module+ test
  (let ([s '("a" "b" "c" "d" "e")])
    (check-equal? (length (ticks-generate (string-ticks s) .1 .9)) 0)
    (check-equal? (length (ticks-generate (string-ticks s) -3 -.1)) 0)
    (check-equal? (length (ticks-generate (string-ticks s) 0 2)) 3)
    (check-equal? (length (ticks-generate (string-ticks s) .1 2)) 2)
    (check-equal? (length (ticks-generate (string-ticks s) 0 10)) 5)
    ))

(module+ drracket
  
  (ticks-generate (string-ticks '("a" "b" "c" "d" "e")) -5 2)
  ; Example of string-ticks
  (parameterize ([plot-y-ticks (string-ticks '("a" "b" "c" "d" "e"))])
    (plot
     (function (λ(x)x) -1 1)
     #:y-min -2 #:y-max 2)))

;; Warning: Contrary to build-list, it starts at 1 and ends at n (not n-1)
(define (build-points f a [b #f] [inc 1])
  (let ([a (if b a 1)]
        [b (if b b a)])
    (for/list ([i (in-range a (+ b 1) inc)]
               #:break (> i b))
      (list i (f i)))))

(module+ test
  (require racket/math)
  (check-equal? (build-points sqr 4)
                '((1 1) (2 4) (3 9) (4 16)))
  (check-equal? (build-points sqr 2 4)
                '((2 4) (3 9) (4 16)))
  (check-equal? (build-points sqr 1 4 2)
                '((1 1) (3 9)))
  (check-equal? (build-points sqr 4 #f 2)
                '((1 1) (3 9)))
  (check-equal? (length (build-points values 0 1 .2))
                6)
  )

;; Useful for calls like:
#;(plot (lines (build-points f 15 45)))

(module+ test
  (check-equal?
   (build-points (λ(x)(* x x)) 1 10 2)
   '((1 1) (3 9) (5 25) (7 49) (9 81))))

;; skip-rec: number of elements to skip in the output list (useful for plots with large lists)
(define (list->points l [start 0]
                      #:skip-rec [skip-rec 0])
  (for/list ([x l]
             [i (in-naturals)]
             #:when (= 0 (modulo i (+ 1 skip-rec))))
    (list (+ i start) x)))

;; Useful for calls like:
#;(plot (lines (list->points '(0.3 0.2 0.75))))

(module+ test
  (check-equal?
   (list->points '(a b c) 2)
   '((2 a) (3 b) (4 c)))
  (check-equal?
   (list->points (range 10) #:skip-rec 2)
   '((0 0) (3 3) (6 6) (9 9)))
  (check-equal?
   (list->points (range 10) 2 #:skip-rec 2)
   '((2 0) (5 3) (8 6) (11 9))))

(define (dict-histogram d)
  (discrete-histogram
   (for/list ([(k v) (in-dict d)])
     (vector k v))))

#; ; Ex:
(plot
   (dict-histogram
   '((a . 10) (b . 20) (c . 12))))

(define-syntax-rule (with-x-log-transform body ...)
  (parameterize ([plot-x-transform log-transform]
                 [plot-x-ticks (log-ticks)])
    body ...))

(define-syntax-rule (with-y-log-transform body ...)
  (parameterize ([plot-y-transform log-transform]
                 [plot-y-ticks (log-ticks)])
    body ...))

(define-syntax-rule (with-xy-log-transform body ...)
  (parameterize ([plot-x-transform log-transform]
                 [plot-y-transform log-transform]
                 [plot-x-ticks (log-ticks)]
                 [plot-y-ticks (log-ticks)])
    body ...))

;; A nicer palette than the default one where the colors are spread across the whole spectrum
;; as much as possible, to avoid color colision as much as possible.
;; (bright colors maybe too bright though)
;; white palette because is build against a white background
(define white-palette
  #((0 0 0)
    (0 139 255)
    (139 255 0)
    (255 0 139)
    (115 115 115)
    (255 115 0)
    (0 255 115)
    (115 0 255)
    (115 231 208)
    (208 115 231)
    (231 208 115)
    (162 0 23)
    (23 162 0)
    (0 23 162)
    (115 0 139)
    (231 0 255)
    (0 139 115)
    (0 255 231)
    (139 115 0)
    (255 231 0)
    (255 115 139)
    (115 139 255)
    (139 255 115)
    (0 0 255)
    (0 255 0)
    (255 0 0)
    (69 69 208)
    (69 208 69)
    (208 69 69)
    (46 69 69)
    (185 185 185)
    (185 23 185)
    (162 185 46)
    (23 185 185)
    (92 46 0)
    (92 185 139)
    (139 92 185)
    (23 0 92)
    (185 231 255)
    (231 255 185)
    (255 185 231)
    (185 139 115)
    (0 115 185)
    (185 0 115)
    (0 92 0)
    (139 69 255)
    (69 255 139)
    (255 139 69)
    (92 0 69)
    (69 231 0)
    (208 255 46)
    (92 139 46)
    (46 208 255)
    (255 46 208)
    (139 46 92)
    (46 92 139)
    (255 0 69)
    (0 69 255)
    (208 162 0)
    (208 46 0)
    (0 208 46)
    (46 0 208)
    (69 139 208)
    (208 69 139)
    (115 185 0)
    (255 185 162)
    (255 255 92)
    (185 162 255)
    (255 92 255)
    (92 255 255)
    (162 255 185)
    (0 69 115)
    (69 115 0)
    (46 255 46)
    (162 115 69)
    (185 231 139)
    (231 139 185)
    (139 185 231)
    (231 185 46)
    (69 23 115)
    (115 162 185)
    (115 69 46)
    (139 162 92)
    (139 208 162)
    (162 139 208)
    (0 46 46)
    (23 115 46)
    (115 231 46)
    (185 92 23)
    (23 185 92)
    (92 23 185)
    (46 23 23)
    (185 115 162)
    (139 0 208)
    (0 208 139)
    (208 46 231)
    (46 231 208)
    (255 69 46)
    (69 46 255)
    (92 255 92)
    (92 115 162)
    (69 115 255)
    (255 69 115)
    (69 139 115)
    (115 69 139)
    (185 231 0)
    (162 92 115)
    (162 46 139)
    (46 139 162)
    (208 139 46)
    (255 162 115)
    (23 255 162)
    (115 255 162)
    (162 23 255)
    (162 115 255)
    (46 69 0)
    (115 0 0)
    (0 185 231)
    (162 208 92)
    (231 0 185)
    (92 69 92)
    (185 23 69)
    (69 185 23)
    (23 46 208)
    (162 255 69)
    (92 185 255)
    (255 92 185)
    (46 46 162))
  #;#((0 0 0)
    (0 169 255)
    (169 255 0)
    (255 0 169)
    (255 84 0)
    (0 255 84)
    (84 0 255)
    (169 169 169)
    (84 84 84)
    (0 0 169)
    (0 169 0)
    (169 0 0)
    (255 84 255)
    (255 255 84)
    (84 255 255)
    (84 255 169)
    (255 169 84)
    (169 84 255)
    (169 0 169)
    (169 169 0)
    (0 169 169)
    (169 169 255)
    (255 169 169)
    (169 255 169)
    (169 255 255)
    (255 255 169)
    (255 169 255)
    (84 84 169)
    (84 169 84)
    (169 84 84)
    (255 0 84)
    (84 255 0)
    (0 84 255)
    (84 169 169)
    (169 169 84)
    (169 84 169)
    (169 0 255)
    (255 169 0)
    (0 255 169)
    (0 255 255)
    (255 255 0)
    (255 0 255)
    (169 0 84)
    (169 84 0)
    (0 169 84)
    (84 169 0)
    (0 84 169)
    (84 0 169)
    (0 84 84)
    (84 0 84)
    (84 84 0)
    (0 0 255)
    (84 84 255)
    (0 255 0)
    (84 255 84)
    (255 0 0)
    (255 84 84)
    (255 84 169)
    (169 255 84)
    (84 169 255)
    (0 0 84)
    (0 84 0)
    (84 0 0)))

(define (palette-ref idx #:palette [palette white-palette])
  (vector-ref palette (modulo idx (vector-length palette))))

(define (make-palette-auto-index #:palette [palette white-palette]
                                 #:start [start 0])
  (define idx start)
  (λ()(begin0 (palette-ref idx #:palette palette)
              (set! idx (add1 idx)))))

#;
(module+ drracket
  (require slideshow racket/draw)

  ;; Example of how to use a palette
  (define get-new-color (make-palette-auto-index))
  (apply
   ht-append
   (for/list ([color (+ 2 (vector-length white-palette))])
     (colorize (filled-rectangle 20 20) (apply make-color (get-new-color)))))

  ;; Helpers to create nice spread palettes

  
  (define (draw-palette palette
                        #:width [width 20]
                        #:border-width [border-width #f]
                        #:background [background #f])
    (apply
     ht-append
     (for/list ([color palette])
       (colorize (filled-rectangle width width
                                   #:border-width border-width
                                   #:border-color (and background (apply make-color background)))
                 (apply make-color color)))))

  (require racket/math)
  (define (color-dist c1 c2)
    (sqrt (apply + (map (compose sqr -) c1 c2))))
  ; use this to find most distant colors
  (define (dist color palette)
    (apply min
           (for/list ([c palette] [i (in-naturals 1)])
             (+ (color-dist c color)
                #;(/ i (+ i 1.)) ; small bonus for old colors
                (/ i))))) ; small bonus for recent colors

  ;; build all color compositions of 0, 63, 127, 191, 255,
  ;; start with white and recursively add the color that is the farther away for the current colors
  ;; (i.e. the current best-response or greedy min-max optimal).
  (define (best-color palette value-list)
    (for*/fold ([best-color #f] [best-dist -1.])
               ([r (in-list value-list)] [g (in-list value-list)] [b (in-list value-list)])
      (define c (list r g b))
      (define d (dist c palette))
      (if (> d best-dist)
          (values c d)
          (values best-color best-dist))))

  ;; Generates a high-contrast palette.
  ;; n-values: number of different values to generat in the list.
  ;; WARNING: Running time is cubic in the number of values! Choose a small number like 4 or maybe 10
  (define (make-spread-palette n-values n-iter [pal '(#;(255 255 255))]
                               #:background [background #f])
    (define value-list
      (build-list n-values (λ(i)(quotient (* 255 i) (- n-values 1)))))
    (reverse
     (for/fold ([pal pal])
               ([iter n-iter])
       (define-values (col dist)
         (best-color (if background (cons background pal) pal) ; make background the most recent color
                     value-list))
       (if (> dist 1.) ; 1 because of bonus
           (cons col pal)
           pal))))

  (for ([background '((255 255 255)
                      (0 0 0)
                      (0 127 127)
                      (255 255 0)
                      (255 0 0)
                      (0 255 0)
                      (0 0 255)
                      )])
    (println
     (draw-palette (make-spread-palette
                    10
                    128
                    #:background background)
                   #:border-width 2
                   #:background background)))

  )




