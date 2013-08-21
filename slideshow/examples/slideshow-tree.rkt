#lang slideshow

(require "../slideshow-utils.rkt"
         "../slideshow-tree.rkt")

; a normal basic tree
(define tree1
  (list "a" ; parent
        ; children
        (list "b" ; parent 
              ; children
              "c")
        (list "d"
              (list "e" "f" "g")
              "h")))

; a "display-tree" where the nodes are displayed with a given style.
; See "Predefined Styles" in slideshow-utils.ss
(define dtree1 (tree-map
                tree1
                (compose (bk-colorizer "gray") 
                         text-double-frame 
                         (colorizer "blue")
                         t)))

; Now display the dtree with different node organizations.
; Press spacebar in the slideshow to go to the next slide.
(slide (draw-tree-top-left-right dtree1))
(slide (draw-tree-left-right dtree1))
(slide (draw-tree-top-down dtree1))
