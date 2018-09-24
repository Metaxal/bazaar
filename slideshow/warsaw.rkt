#lang racket/base
;;; Copyright (C) Laurent Orseau, 2010-2013
;;; GNU Lesser General Public Licence (http://www.gnu.org/licenses/lgpl.html)

(require (rename-in slideshow 
                    ; redefinitions:
                    [item item-old]
                    [subitem subitem-old]
                    [slide slide-old]
                    )
         racket/draw/private/color
         "slideshow-utils.rkt"
         "../draw/color.rkt"
         )

;; Warsaw style

(provide red-sphere-bullet green-sphere-bullet blue-sphere-bullet 
         cyan-sphere-bullet yellow-sphere-bullet magenta-sphere-bullet
         black-sphere-bullet gray-sphere-bullet
         color-bullet color-subbullet
         current-item-bullet current-subitem-bullet
         current-body-color current-background-color
         current-left-color current-right-color
         text-button
         init-slide-style
         title-slide
         ; Re-exports:
         slide
         item subitem
         (except-out (all-from-out slideshow) 
                     item-old subitem-old slide-old)
         )

(set-margin! 0)

(define-syntax-rule (send/values obj (meth args ...) ...)
  (values
   (send obj meth args ...)
   ...))

(define (fint x) (inexact->exact (floor x)))

;; Returns a function that fades from color1 to color2
;; when n goes from 0.0 to 1.0
;; and returns a color% object.
(define ((color-fader c1 c2) n)
  (let-values ([(r1 g1 b1) (send/values c1 (red) (green) (blue))]
               [(r2 g2 b2) (send/values c2 (red) (green) (blue))])
    (make-object color%
      (fint (+ r1 (* n (- r2 r1))))
      (fint (+ g1 (* n (- g2 g1))))
      (fint (+ b1 (* n (- b2 b1))))
      )))

(define (make-color-parameter vinit)
  (let ([guard (λ(v)(if (is-a? v color%)
                       v
                       (or (send the-color-database find-color v)
                           (send the-color-database find-color "white"))))])
    (make-parameter (guard vinit) guard)))

(current-title-color (make-object color% 255 255 255)) ; must be a color%, not a string!
(define current-body-color (make-color-parameter "black"))
(define current-background-color (make-color-parameter "white"))
(define current-left-color (make-color-parameter "black"))
(define current-right-color (make-color-parameter (make-object color% 51 51 179)))
(current-font-size 28)
;(current-line-sep 8)

(define (make-bullet-parameter vinit)
  (let ([guard (λ(v)(cond [(pict? v) v]
                          [(equal? v 'color-bullet)
                           (color-bullet (current-right-color))]
                          [(equal? v 'color-subbullet)
                           (color-subbullet (current-right-color) 
                                            (current-background-color))]
                          [else bullet]))])
    (make-parameter (guard vinit) guard)))

(define current-item-bullet (make-bullet-parameter bullet)) ; redefined later
(define current-subitem-bullet (make-bullet-parameter o-bullet)) 

(define (init-slide-style pres-title authors)
  (let* ([title-zone 
          (make-hz-fade-rectangle client-w 60 50 
                                  (color-fader (current-right-color) 
                                               (current-left-color)))]
         [title-shade
          (make-vt-fade-rectangle client-w 10 20
                                  (λ(n)(let ([n (+ .5 (* .5 n))])
                                         (rgb->color n n n))))]
         [slide-bk
          (do-with-id 
           it (colorize (filled-rectangle client-w client-h) (current-background-color))
           (lt-superimpose
            it
            (hc-append (colorize (filled-rectangle (/ client-w 2) 25) 
                                 (current-left-color))
                       (colorize (filled-rectangle (/ client-w 2) 25) 
                                 (current-right-color)))
            )
           (lb-superimpose
            it
            (hc-append
             (rc-superimpose
              (colorize (filled-rectangle (/ client-w 2) 25) (current-left-color))
              (hc-append (colorize (text authors (current-main-font) 16) 
                                   (current-title-color))
                         (blank 20 1)))
             (lc-superimpose 
              (colorize (filled-rectangle (/ client-w 2) 25) (current-right-color))
              (hc-append (blank 20 1)
                         (colorize (text pres-title (current-main-font) 16) 
                                   (current-title-color))))))
           )])
    (current-slide-assembler
     (λ(title sep pict)
       (lt-superimpose
        slide-bk
        (if title 
            (at 0 25 
                (vl-append
                 (lc-superimpose
                  title-zone
                  (hc-append (blank 40 60) (titlet (or title "")))
                  )
                 title-shade))
            (blank))
        (at 0 85 pict)))))
  )


(define (make-sphere w color-proc)
  (for/fold ([pic (blank)])
    ([i (in-range 0 w)])
    (let ([j (* i .333)])
      (lt-superimpose
       pic
       (at j j
           (colorize 
            (disk (- w i))
            (color-proc (/ i (+ 0. w)))))))))

;; color: (one-of 'red 'green 'blue 'cyan 'yellow 'magenta 'black 'gray)
(define (make-sphere-bullet [color 'blue]  [size 13])
  (let ([bproc 
         (case color
           [(red)     (λ(n1 n2)(list n1 n2 n2))]
           [(green)   (λ(n1 n2)(list n2 n1 n2))]
           [(blue)    (λ(n1 n2)(list n2 n2 n1))]
           [(cyan)    (λ(n1 n2)(list n2 n1 n1))]
           [(yellow)  (λ(n1 n2)(list n1 n1 n2))]
           [(magenta) (λ(n1 n2)(list n1 n2 n1))]
           [(black)   (λ(n1 n2)(list n2 n2 n2))]
           [(gray)    (λ(n1 n2)(list n1 n1 n1))]
           )])
    (baseless
     (cc-superimpose (blank 0 gap-size) 
                     (make-sphere size
                                  (λ(n)(let ([n1 (+ .3 (* .7 n))]
                                             [n2 (* n n)])
                                         (apply rgb->color (bproc n1 n2)))))
                     ))))

(define (make-shaded-sphere [color-str 'blue]  [size 50])
  (lt-superimpose
   (at (/ size 3.) (* size .95) 
       (colorize (filled-ellipse size (/ size 10.)) "darkgray"))
   (make-sphere-bullet color-str size)))

(define-values
  (red-sphere-bullet green-sphere-bullet blue-sphere-bullet 
                     cyan-sphere-bullet yellow-sphere-bullet magenta-sphere-bullet
                     black-sphere-bullet gray-sphere-bullet)
  (apply values
         (map make-sphere-bullet
              '(red green blue cyan yellow magenta black gray))))

(define (color-bullet c)
  (baseless (cc-superimpose
             (blank 0 gap-size)
             (colorize (disk 10) c)
             )))

(define (color-subbullet c cbk)
  (baseless (cc-superimpose (blank 0 gap-size)
                            (colorize (disk 12) c)
                            (colorize (disk 6) cbk))))

(current-item-bullet blue-sphere-bullet)

;; Redefines the default bullet for item
(define (item #:bullet [blt (current-item-bullet)]
              #:width [width (current-para-width)]
              #:align [align 'left]
              #:fill? [fill? #t]
              #:decode? [decode? #t]
              . l)
  (apply item-old 
         #:width width	 	 	 	 
         #:bullet blt	 	 	 	 
         #:align align	 	 	 	 
         #:fill? fill?	 	 	 	 
         #:decode? decode?
         l))

(define (subitem #:bullet [blt (current-subitem-bullet)]
              #:width [width (current-para-width)]
              #:align [align 'left]
              #:fill? [fill? #t]
              #:decode? [decode? #t]
              . l)
  (apply subitem-old 
         #:width width	 	 	 	 
         #:bullet blt	 	 	 	 
         #:align align	 	 	 	 
         #:fill? fill?	 	 	 	 
         #:decode? decode?
         l))


;; Writes text in color-right (default) 
;; in a shadowed filled-rounded-rectangle with current-title-color (default).
;; -> pict
(define (text-button str [str-color (current-title-color)]
                     [button-color (current-right-color)]
                     #:inset-w [inset-w 10]
                     #:inset-h [inset-h 2]
                     #:shade-dh [shade-dh 5]
                     #:shade-dv [shade-dv shade-dh]
                     #:shade-color-scale [scscale 1.3]
                     )
  (let* ([txt (inset (t str) inset-w inset-h)]
         [w (pict-width txt)]
         [h (pict-height txt)]
         )
    (inset 
     (baseless
      (cc-superimpose
       (at shade-dv shade-dh
           (colorize
            (filled-rounded-rectangle w h)
            (scale-color scscale (current-right-color))
            ))
       (cc-superimpose
        (colorize
         (filled-rounded-rectangle w h)
         (current-right-color))
        (colorize txt str-color))
       ))
     0 (/ gap-size 2) 0 0
     )))

(define (title-slide title . l)
  (apply slide #:title #f
         (bigger (bigger (text-button title #:inset-w 80 #:inset-h 40
                                      #:shade-dv 10 #:shade-dh 10
                                      )))
         (blank 0 (* gap-size 2))
         l))


(define (slide #:title      [title #f]
 	 	#:name      [name title]
 	 	#:layout    [layout 'auto]
 	 	#:inset     [inset (make-slide-inset 0 0 0 0)]
 	 	#:timeout   [secs #f]
 	 	#:condense? [condense? (and secs #t)]
 	 	. l)
  (apply slide-old
         #:title     title
         #:name      name
         #:layout    layout
         #:inset     inset
         #:timeout   secs
         #:condense? condense?
         (map (λ(e)(if (pict? e) (colorize e (current-body-color)) e))
              l)))

(module+ main

; also see interactive-style-maker.rkt

; When in a separate file, require like this:
; #lang racket/base
; (require bazaar/slideshow/warsaw) ; replaces slideshow

(init-slide-style "My title" "Me and someone else")

(slide #:title "The Generic Algorithm"
       (item "Start with something")
       'next
       (item "Turn it into something else")
       'next
       (item "And you're done!")
       'next
       (t "How easy is that?")
       )
)