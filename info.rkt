#lang setup/infotab

(define deps
 '("base"
   "data-lib"
   "define2"
   "draw-lib"
   "gui-lib"
   "images"
   "math-lib"
   "net-lib"
   "plot-gui-lib"
   "plot-lib"
   "racket-index"
   "rackunit-lib"
   "scribble-lib"
   "slideshow-lib"
   "srfi-lite-lib"
   ))

(define test-omit-paths
  '("slideshow/examples"
    "gui/examples"))