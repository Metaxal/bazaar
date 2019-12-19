#lang slideshow
;;; Copyright (C) Laurent Orseau, 2010-2013
;;; GNU Lesser General Public Licence (http://www.gnu.org/licenses/lgpl.html)

(require "slideshow-utils.rkt"
         "../tree.rkt")

(provide (all-defined-out)
         (all-from-out "../tree.rkt"))

;;; Also see pict/tree-layout

;;; A tree follows the (parent . children) representation.

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Drawing Trees   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(define (draw-tree tree appender
                   [draw-over (λ(scene parent child)scene)] )
  (if (list? tree)
      (let* ( [node (first tree)]
              [child-pics (map (λ(n)(draw-tree n appender
                                               draw-over))
                               (rest tree))]
              [scene (appender node child-pics)] )
        (foldl (λ(child scene)(draw-over scene (first tree) 
                                         (if (list? child)(first child) child)))
               scene
               (rest tree)))
      tree))
      
;=============================;
;=== Some predefined trees ===;
;=============================;

(define (draw-tree-top-down tree [td-w 10] [lr-w td-w] )
  (draw-tree 
   tree
   (λ(parent children)(vc-append parent (blank 0 td-w) 
                                 (apply ht-append 
                                        (add-between children (blank td-w 0)))))
   pin-line-top-down
   ))

(define (draw-tree-left-right tree [td-w 10] [lr-w td-w] 
                              #:pin-line [pin pin-line-left-right])
  (draw-tree 
   tree
   (λ(parent children)(hc-append parent (blank td-w 0)
                                 (apply vl-append
                                        (add-between children (blank 0 lr-w)))))
   pin
   ))

(define (draw-tree-top-left-right tree [td-w 10] [lr-w td-w] )
  (draw-tree 
   tree
   (λ(parent children)
     (ht-append parent (blank td-w) 
                (apply vl-append (ghost parent)
                       (add-between children (blank 0 lr-w)))))
   (λ(scene a b)
     (let-values ([(ax ay) (cb-find scene a)]
                  [(bx by) (lc-find scene b)])
       (lt-superimpose
        scene
        (at ax ay (rectangle 2 (- by ay)))
        (at ax by (rectangle (- bx ax) 2))
        )))
   ))

(define (draw-tree-left-right-down tree [td-w 10] [lr-w td-w] )
  (draw-tree 
   tree
   (λ(parent children)
     (vl-append parent (blank lr-w) 
                (apply ht-append (ghost parent)
                       (add-between children (blank td-w 0)))))
   (λ(scene a b)
     (let-values ([(ax ay) (rc-find scene a)]
                  [(bx by) (ct-find scene b)])
       (lt-superimpose
        scene
        (at ax ay (rectangle (- bx ax) 2))
        (at bx ay (rectangle 2 (- by ay)))
        )))
   ))

(module+ drracket
  (current-font-size 12)
  (define t1 '(a (b1 (c1 d1 d2) (c2 d3 d4 d5)) 
                 (b2 (c3 d6) c4)))
  (define t1-t (tree-map t1 (λ(x)(t (symbol->string x))) identity))
  (displayln "top-down")
  (draw-tree-top-down         t1-t 20 10)
  (displayln "left-right")
  (draw-tree-left-right       t1-t 40 20)
  (displayln "left-right-down")
  (draw-tree-left-right-down  t1-t 50 0)
  (displayln "top-left-right")
  (draw-tree-top-left-right   t1-t 10 0)
  )
