#lang slideshow
;;; Copyright (C) Laurent Orseau, 2010-2013
;;; GNU Lesser General Public Licence (http://www.gnu.org/licenses/lgpl.html)

(require "../slideshow-utils.rkt"
         "../slideshow-table.rkt")

; tests :
(define t2 (map-table-xy 
            (compose (inseter 5)
                     (tableA-header-body-style 
                      (λ(x y e)(bigger (emph (t e))))
                      (λ(x y e)(smaller (bold (t e))))))
                         '(("a" "bbb" "cc") ("dd" "e" "ffff") ("ggg" "hh" "i"))))
(define t1 (list 
            (list (vl-append (t "ac") (t "plop")) (t "betta") (t "tetuta"))
            (list (t "c") (t "d") (t "hoyyoooo"))
            (list (t "youpladouba") (t "pliacaplouc") 
                  (vc-append (t "youhou") (t "nope")))))

(slide (tableA-double-framed t1))

(slide
 (bk-colorize
  (tableA-double-framed 
   (map-table (inseter 5) t2)
   (compose 
    (bk-colorizer "dark green")
    (inseter 5)
    (tableA-header-body-style 
     (compose (bk-colorizer "red")(inseter 5)(bk-colorizer "pink")
              (colorizer "blue") tableA-cc-style)
     (compose (bk-colorizer "gray")(inseter 5)
              (tableA-left-right-style 
               (compose (colorizer "red") tableA-rc-style)
               tableA-lc-style)))))
  "yellow"))
