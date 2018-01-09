#lang slideshow
;;; Copyright (C) Laurent Orseau, 2010-2013
;;; GNU Lesser General Public Licence (http://www.gnu.org/licenses/lgpl.html)

(require "../list.rkt"
         "slideshow-utils.rkt")

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Tables with Styles   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (same-width pics)
  (let ([max-w (apply max (map pict-width pics))])
    (map (λ(p) (blank max-w (pict-height p)))
         pics)))
(define (same-height pics )
  (let ([max-h (apply max (map pict-height pics))])
    (map (λ(p)(blank (pict-width p) max-h))
         pics)))



;  tableA-style : (int int pict pict) -> pict

; tableA-style -> (pic . pic -> pic)
; applies the same style to all cells
; 
(define (tableA-style-all style)
  (λ(x y . r) (apply style r))) ; r can be (ghost-pic pic) or (text)

(define tableA-superimposer tableA-style-all) ; alias

(define tableA-cc-style (tableA-style-all cc-superimpose))
(define tableA-lc-style (tableA-style-all lc-superimpose))
(define tableA-rc-style (tableA-style-all rc-superimpose))


; (list (list any)) -> (list (list any))
; inner lists must all have the same size
(define (map-table proc . tabs)
  (apply map (curry map proc) tabs))
; test:
; (map-table + '((1 2 3) (4 5 6)) '((10 20 30) (40 50 60)))


(define (map-table-xy proc . tabs)
  (apply map 
         (λ(x . rows)(apply map proc 
                            (build-list (length (first rows)) (λ(y)x))
                            (build-list (length (first rows)) values)
                            rows))
         (build-list (length (first tabs)) values)
         tabs))
; test :
;(map-table-xy + '((100 200 300) (400 500 600))
;                '((1000 2000 3000) (4000 5000 6000)))

; Draws the pics in a table (resizes correctly the widths and heights).
; cell-style is applied to each row after resizing.
; cell-style : tableA-style
; pics : (list (list pict))
(define (tableA pics [cell-style tableA-cc-style] )
  ; pics : (listof (listof pic)) ; a list of lines 
  (let* ( [h-aligned (map same-height pics)] 
          [hpics (transpose h-aligned)]
          [pics-blank (transpose (map same-width hpics))]
          [tab (map-table-xy cell-style pics-blank pics)] )
    (apply vl-append
           (map (λ(lpic)(apply hc-append lpic)) tab))))

; Meta-styles
; styles that can be applied to styles to compose them

; tableA-style-splitter : tableA-style . tableA-style -> tableA-style

; tableA-style-splitter
(define (tableA-if-style test true-style false-style)
  (λ(x y . r)(if (apply test x y r) 
                 (apply true-style x y r)
                 (apply false-style x y r))))

; tableA-style-splitter
(define (tableA-header-body-style header-style body-style)
  (tableA-if-style (λ(x y . r)(= 0 x)) header-style body-style))
; tableA-style-splitter
(define (tableA-left-right-style left-style right-style)
  (tableA-if-style (λ(x y . r)(= y 0)) left-style right-style))

; tableA-style-splitter
(define (tableA-odd-even-row-style odd-style even-style)
  (tableA-if-style (λ(x y . r)(odd? x)) odd-style even-style))

; tableA-style-splitter
(define (tableA-odd-even-column-style odd-style even-style)
  (tableA-if-style (λ(x y . r)(odd? y)) odd-style even-style))

; tableA-style-splitter
(define (tableA-checker-style odd-style even-style)
  (tableA-if-style (λ(x y . r)(odd? (+ x y))) odd-style even-style))

; faire une fonction qui prend 3 paramètres et qui renvoie la bonne fonction
;(define (place-pict pic1 pic2 )

(define (tableA-items-style style)
  (tableA-left-right-style
   (tableA-style-all rt-superimpose)
   (tableA-style-all lt-superimpose)))

; Predefined tableAs :

(define (tableA-header-body pics header-style [body-style tableA-cc-style])
  (tableA pics (tableA-header-body-style header-style body-style)))

(define (tableA-left-right pics left-style [right-style tableA-cc-style])
  (tableA pics (tableA-left-right-style left-style right-style)))

; FAIRE un mécanisme générique pour définir des styles et les composer !
; les styles sont des fonctions !

; C'est un peu comme des composants !
; il s'emboitent seulement s'ils ont les bonnes encoches.
; ici on a des styles qui ne consomme rien, qui ne font que transformer
; le fond sans changer les interfaces

(define (tableA-items pics)
  (tableA-left-right pics 
                     (tableA-style-all rt-superimpose)
                     (tableA-style-all lt-superimpose)))

(define (tableA-framed pics [style tableA-cc-style])
  (tableA pics (compose frame (λ(p)(inset p 3)) style)))

(define (tableA-double-framed pics [style tableA-cc-style]
                              #:inset [inset-size 3])
  (frame 
   (inset
    (tableA pics
            (compose (λ(p)(inset p (/ inset-size 2)))
                     frame style))
    (/ inset-size 2))))
