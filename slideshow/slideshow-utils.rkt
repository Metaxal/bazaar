#lang slideshow

(require racket/draw ; for color%
         "../draw/color.rkt")

(provide (all-defined-out))

; can also be used in map, table-map, etc.
; color -> (pict -> pict)
(define (colorizer color)
  (λ(p)(colorize p color)))

(define (bk-colorizer color)
  (λ(p)(bk-colorize p color)))

; (int ...) -> (pict -> pict)
(define (inseter . vals) ; because inset is defined with different arities!
  (λ(p)(apply inset p vals)))

(define text-frame (compose frame (inseter 5)))
(define text-double-frame (compose frame (inseter 2) frame (inseter 5)))

(define (rounded-frame p
                       #:inset [in 2] #:corner-radius [radius -0.25]
                       #:angle [angle 0])
  (cc-superimpose p (rounded-rectangle (+ (pict-width p) in) (+ (pict-height p) in)
                                       radius #:angle angle)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   And Now for Something Completely Different   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; splits lines by "\n" and append them vertically 
(define (string->t-lines str [appender vl-append] [texter t])
  (apply appender
         (map texter
              (string-split str "\n"))))
 

;(define (bitmap-scaled file)
;  (scale (bitmap file) 0.96))
;; strangely, pictures are not properly scale in the slideshow!
;; 0.96 = 768/800 = slideshow resolution / my resolution 

; useful for (arrow gap-size left) etc.
(define right 0)
(define left pi)
(define up (/ pi 2))
(define down (- (/ pi 2)))

; size-parameterized arrows
(define (arrow-sized angle) (arrow (current-font-size) angle))
(define (arrowhead-sized angle) (arrowhead (current-font-size) angle))

; the size of this bullet is autoadjested to the font size
(define-syntax arrowhead-bullet
  (syntax-id-rules ()
    [_ (arrowhead-sized right)]))

(define-syntax arrow-bullet
  (syntax-id-rules ()
    [_ (arrow-sized right)]))


; We should have parameters for item/subitem/subsubitem arrows
; so that the style can be defined at the top of the program for the rest of the code


; useful to generate things automatically
(define append-places '(vl vc vr ht htl hc hbl hb))
(define superimpose-places '(lt ltl lc lbl lb ct ctl cc cbl cb rt rtl rc rbl rb))
(define pict-places (append append-places superimpose-places))

(define (place-pict where . pics)
  (apply
   (match where
     ['vl vl-append]
     ['vc vc-append]
     ['vr vr-append]
     ['ht ht-append]
     ['htl htl-append]
     ['hc hc-append]
     ['hbl hbl-append]
     ['hb hb-append]
     ['lt lt-superimpose]
     ['ltl ltl-superimpose]
     ['lc lc-superimpose]
     ['lbl lbl-superimpose]
     ['lb lb-superimpose]
     ['ct ct-superimpose]
     ['ctl ctl-superimpose]
     ['cc cc-superimpose]
     ['cbl cbl-superimpose]
     ['cb cb-superimpose]
     ['rt rt-superimpose]
     ['rtl rtl-superimpose]
     ['rc rc-superimpose]
     ['rbl rbl-superimpose]
     ['rb rb-superimpose]
     [_ (error "unknown place in place-pict:" where)]
     ) pics))




; taken from interlocking-components.scm
(define (at x y pict) (hc-append (blank x 0) (vc-append (blank 0 y) pict)))
; could be used with lt-superimpose


(define (pict-area pict)
  (* (pict-height pict) (pict-width pict)))

;;;;;;;;;;;;;;;;;;;;;;;
;;;   Pict Stager   ;;;
;;;;;;;;;;;;;;;;;;;;;;;

; Use slideshow/steps instead when possible !
; but this may allow for more complex tuning.
; 'alts for complex picture placements
; keeps the place of picts that will be drawn later (ghosts them)
; -> layout is preserved

; nmax : number
; proc : number -> pic
(define (pic-alts nmax proc)
  (build-list nmax (compose list proc)))

; test : boolean
; pic : pict
(define (if-alt test pic) ; TODO: rename to when-alt !
  (if test pic (ghost pic)))

;; usage example  :
;(slide
; 'alts
; (pic-alts 
;  5 (λ(n) (vc-append 
;           (t "a")
;           (t (number->string n))
;           (hc-append
;            (t "aaaaa")
;            (if-alt (> n 0) (t "bbb"))
;            (t "aaaaa")
;            (if-alt (< n 3) (t "c"))
;            (t "aaa"))
;           (if-alt (= 3 n) (t "d"))
;           (t "aaaaa"))))
; )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Text Style Macros   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Warning:
; --------
; The following commands for controlling the text style only work
; for text that has not yet been printed, e.g. this won't work:
;(let ( [x (t "a")] )
;    (hbl-append x (smaller x) (bigger x)))
;
; but this will:
;(let ( [x (λ()(t "a"))] )
;    (hbl-append (x) (smaller (x)) (bigger (x))))

; temporary sets a new-style to style-proc 
; and executes proc with this style.
; Needs to be a macro to avoid evaluating proc beforehand
(define-syntax-rule (temp-style style-proc new-style body ...)
  (parameterize ([style-proc new-style])
    body ...
    ))

; predefined styles
; no need for "..." (could even be dangerous)
; because only the last proc would be returned/rendered
(define-syntax-rule (bold body ...)
  (parameterize ([current-main-font (cons 'bold (current-main-font))])
    body ...
    ))

(define-syntax-rule (emph body ...)
  (parameterize ([current-main-font (cons 'italic (current-main-font))])
    body ...
    ))

(define-syntax-rule (font-size n body ...)
  (parameterize ([current-font-size n])
    body ...
    ))

; temporary Bigger font
; Warning ! (bigger (smaller ...)) may not be exactly the same as (smaller (bigger ...)) !
; it should not be visible however if there are jsut a few imbrications
; (we could use an external non-integer exact number for that)
(define-syntax bigger
  (syntax-rules ()
    [(_ proc)
     (parameterize ([current-font-size (round (* (/ 4 3) (current-font-size)))])
       proc)]
    [(_ font proc)
     (parameterize ([font (round (* (/ 4 3) (font)))])
       proc)]
    ))
;(define-syntax-rule (bigger proc)
;  (parameterize ([current-font-size (round (* (/ 4 3) (current-font-size)))])
;    proc
;    ))

(define-syntax smaller
  (syntax-rules ()
    [(_ body ...)
     (parameterize ([current-font-size (round (* (/ 3 4) (current-font-size)))])
       body ...)]
    [(_ font body ...)
     (parameterize ([font (round (* (/ 3 4) (font)))])
       body ...)]
    ))
;(define-syntax-rule (smaller proc)
;  (parameterize ([current-font-size (round (* (/ 3 4) (current-font-size)))])
;    proc
;    ))

; Like parameterize (temp-style), but for non-parameters (see below)
(define-syntax-rule (temp-style2 style-proc new-style body ...)
  ; style-proc : 
  ;    (<proc>) -> any : gets the old style value
  ;    (<proc> x) -> void : sets the new style value
  (let ([old-style (style-proc)])
    (style-proc new-style)
    (begin0 body ...
            (style-proc old-style))))

; cannot use this with parameterize, so use it with temp-style2
; faire un make-parameter avec une fonction de modif !
(define (current-title-h [h #f])
  (if h
      (set-title-h! h)
      title-h))

(define (blank-line) (blank 0 (current-font-size)))
(define (blank-sized) (blank (current-font-size) (current-font-size)))

;; Returns a rectangle of size width*height
;; filled with a color pattern decomposed in blocks of steps colors.
;; width: integer-in 0 10000
;; height: integer-in 0 10000
;; steps: integer-in 0 10000
;; color-proc: (real-in 0.0 1.0) -> color%
;; Tip: Use rgb->color
(define (make-hz-fade-rectangle width height steps color-proc)
  (for/fold ([pic (blank 0)])
    ([i (in-range 0 steps)])
    (hc-append pic (colorize (filled-rectangle (/ width steps) height)
                             (color-proc (/ (exact->inexact i) steps))))))

#| TEST | #
(make-hz-fade-rectangle
 100 10 10
 (λ(n)(rgb->color n 0 1)))
(make-hz-fade-rectangle
 400 30 80
 (λ(n)(rgb->color n 1 (- 1. n))))

;|#

;; Like make-hz-fade-rectangle but vertical instead of horizontal.
(define (make-vt-fade-rectangle width height steps color-proc)
  (let ([steps (exact->inexact steps)])
    (for/fold ([pic (blank 0)])
      ([i (in-range 0 steps)])
      (vc-append pic (colorize (filled-rectangle width (/ height steps))
                               (color-proc (/ i steps)))))))
#| TEST | #
(make-vt-fade-rectangle
 100 10 10
 (λ(n)(rgb->color n 0 1)))
(make-vt-fade-rectangle
 400 30 80
 (λ(n)(rgb->color n 1 (- 1. n))))

;|#

;(define (text-frame t)
;  (frame (inset t 5))) 

(define (list-t . pics)
  (map t pics))

; colorizes the background of a picture
(define (bk-colorize pic color)
  (lt-superimpose
   (colorize
    (filled-rectangle (pict-width pic)
                      (pict-height pic))
    color)
   pic))

; colorizes the background of pic only if it's a single line
(define (bk-colorize-line pic color)
  (if (< (pict-height pic) (* 1.5 (current-font-size)))
      (bk-colorize pic color)
      pic))

; doubles the picture with an offset and a color lighten factor
(define (cast-shade pic [color "black"] [factor 1.3] [vinset 3])
  (lt-superimpose (colorize pic (scale-color factor color)) 
                  (colorize (inset pic (- vinset) vinset) color)))

; fun one
; could use color-series ?
(define (deepen pic nsteps [rfactor 1] [gfactor 1] [bfactor 1]
                #:superimposer [superimposer ct-superimpose]
                #:scale [sfactor .01])
  (apply
   superimposer
   (build-list nsteps
               (λ(n)
                 (colorize 
                  (scale pic (+ 1 (* sfactor n)))
                  (make-object color%
                    (min 255 (* rfactor n))
                    (min 255 (* gfactor n))
                    (min 255 (* bfactor n)))))
               )))
; Try this:
; (bk-colorize (deepen (t "Information") 255 1 3 9) "light blue")

(define make-pin-line
  (make-keyword-procedure
   (λ(kws kw-args find1 find2)
     (λ(scene pic1 pic2)
       (keyword-apply pin-line kws kw-args scene pic1 find1 pic2 find2 '())))))

(define pin-line-top-down (make-pin-line cb-find ct-find))
#;(define (pin-line-top-down scene pic-top pic-down)
  (pin-line scene 
            pic-top  cb-find
            pic-down ct-find))

(define pin-line-left-right (make-pin-line rc-find lc-find))
#;(define (pin-line-left-right scene pic-left pic-right)
  (pin-line scene 
            pic-left  rc-find
            pic-right lc-find))


;;;;;;;;;;;;;;;;;;
;;;   Tables   ;;;
;;;;;;;;;;;;;;;;;;
; put this in "slideshow-table.ss" ?

; predefined style for a table where the left column is aligned to the right
; and the right is aligned to the left
(define (table-items l)
  (table 2 l
         (list rc-superimpose lc-superimpose)
         cc-superimpose
         20 1))

;;;;;;;;;;;;;;;;;;;;;;;
;;;   Bitmap/pict   ;;;
;;;;;;;;;;;;;;;;;;;;;;;

(define (pict->bmp p [bkg-color "white"] [scale 1])
  (let* ([bmp
          (make-object bitmap%
            (inexact->exact (ceiling (pict-width p)))
            (inexact->exact (ceiling (pict-height p))))]
         [dc (make-object bitmap-dc% bmp)])
    (send dc set-background (color/string->color bkg-color))
    (send dc clear)
    (send dc set-scale scale scale)
    (draw-pict p dc 0 0)
    (send dc set-bitmap #f)
    bmp))

(define (pict-save-file pict file kind [quality 75] [bkg-color "white"] [scale 1])
  (let ([bmp (pict->bmp pict bkg-color scale)])
    (if (send bmp ok?)
        (send bmp save-file file kind quality)
        (error "bitmap not ok for file" file))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Scene modifications   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; boxes that are pictures, even initially
(define (blank-box) (box (blank)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; superimposes pict-over over pict-in-scene in scene if found in scene

(define (pin-pict-over scene pict-in-scene pict-over [appender cc-superimpose]
                       [dx 0] [dy 0])
  (let*-values ( [(pic-ghost) (ghost pict-in-scene)]
                 [(local-pic) (appender pic-ghost pict-over)]
                 [(xs ys) (lt-find scene pict-in-scene)]
                 [(xl yl) (lt-find local-pic pic-ghost)] )
    (panorama (lt-superimpose scene (at (+ (- xs xl) dx) (+ (- ys yl) dy)  local-pic)))))

; generic function to compose applications to a initial value.
; this one needs (λ(x)...) for each proc
(define (do-with x . procs)
  (foldl (λ(proc x)(proc x)) x procs))
;;  ((apply compose (reverse procs)) x)) ; same definition

; with a macro
; no need for lambdas, but require an identifier for id
; init is the initial value
(define-syntax-rule (do-with-id id init proc ...)
  (let ([id init])
    (foldl (λ(p x)(p x)) id (list (lambda(id)proc) ...))))

; Usage example:
;(let* ( [pic1 (t "a")]
;        [pic2 (t "b")]
;        [scene (vc-append pic1 pic2)] )
;  (list (frame scene)
;        (do-with-id
;         scene scene
;         (pin-pict-over scene pic2 (bk-colorize pic2 "yellow"))
;         (pin-pict-over scene pic1 (colorize
;                                    (frame (inset pic1 3) #:color "green")
;                                    "blue"))
;         (pin-pict-over scene pic2 (colorize (t "x") "red") hc-append)
;         (pin-pict-over scene pic2 (colorize (t "x") "purple") 
;                        (λ(x y)(hc-append y x))) ; place to the left
;         (frame (clip scene))
;         )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; To draw several pin-arrow-lines for one same scene
; (composes the application 
; + allow to use predefined arrow-styles)

; scene : pict
; larrows : (list (list pict pict-finder pict pict-finder)
(define-struct arrow-style
  (size solid? line-width color under?))

; predefined arrow styles
(define arrow-light (make-arrow-style 10 #f 1 "black" #f))
(define arrow-red (struct-copy arrow-style arrow-light [color "red"] [line-width 2]))
; e.g., use : (define my-arrow (struct-copy arrow-style arrow-light [color "blue"]))
; to extend an existing arrow style

; we could create a parameter for current-arrow-style
; and use also a label #:arrow-style [arrow (current-arrow-style)] in functions.
; -> arrows can be parameterized from high above or with a single value in functions.

; add an arrow from src to dst if found in scene
(define (scene-arrow scene arrow src find-src dst find-dst)
  (pin-arrow-line (arrow-style-size arrow)
                  #:color (arrow-style-color arrow)
                  #:line-width (arrow-style-line-width arrow)
                  #:solid? (arrow-style-solid? arrow)
                  #:under? (arrow-style-under? arrow)
                  scene src find-src dst find-dst))

; to add multiple arrows at a time
(define (scene-arrows scene . larrows)
  (foldl (λ(arr-pics scene)
           (apply scene-arrow scene arr-pics))
         scene
         larrows))

;; Usage example :
;(let* ( [pic1 (frame (t "a"))]
;        [pic2 (frame (t "b"))]
;        [pic3 (frame (t "c"))]
;        [scene   (hc-append (vc-append pic1
;                                      (blank 200)
;                                      pic2)
;                           pic3)]
;        [a arrow-light])
;  (scene-arrows scene
;   (list a pic1 cb-find pic2 ct-find)
;   (list a pic2 rc-find pic3 cb-find)
;   (list a pic3 ct-find pic1 rc-find))
;  )

(define (ghost-frame pic
                     #:color [color "black"] #:line-width [line-w 1]
                     #:inset [inset-w 0])
  (frame (inset (ghost pic) inset-w) 
                      #:color color #:line-width line-w))
(define (red-ghost-frame pic)
  (ghost-frame pic #:inset 2 #:color "red" #:line-width 2))

(define (frame-arrow-over scene src dst [src-find cc-find] [dst-find cc-find]
                          #:color [color "black"] #:line-width [line-w 1]
                          #:inset [inset-w 0])
  (let ( [pic1 (ghost-frame src #:color color #:line-width line-w #:inset inset-w)]
         [pic2 (ghost-frame dst #:color color #:line-width line-w #:inset inset-w)]
         [arr (struct-copy arrow-style arrow-light [color color] [line-width line-w])] )
    (do-with-id 
     scene scene
     (pin-pict-over scene src pic1)
     (pin-pict-over scene dst pic2)
     (scene-arrow scene arr pic1 src-find pic2 dst-find)
     )))

(define (frame-over scene pic 
                    #:color [color "black"] #:line-width [line-w 1]
                    #:inset [inset-w 0])
  (pin-pict-over scene pic (frame (inset (ghost pic) inset-w) 
                                  #:color color #:line-width line-w)))

(define (red-frame-over scene pic)
  (frame-over scene pic #:color "red" #:line-width 2 #:inset 2))

(define (red-frame-arrow-over scene src dst [src-find cc-find] [dst-find cc-find])
  (frame-arrow-over scene src dst src-find dst-find
                     #:color "red" #:line-width 2 #:inset 2))

; Example:
;(let* ([x (t "a")]
;       [y (t "b")]
;       [scene
;        (cc-superimpose (blank 100 100)
;                        (at 5 20 x)
;                        (at 80 40 y))])
;  (list
;   (frame-over y (red-frame-over scene x) #:color "blue" #:inset 10)
;   (frame-arrow-over scene x y)
;   (red-frame-arrow-over scene x y)
;  ))