#lang racket/base 
;;; Copyright (C) Laurent Orseau, 2010-2013
;;; GNU Lesser General Public Licence (http://www.gnu.org/licenses/lgpl.html)

(require "../matrix.rkt"
         racket/gui/base
         racket/class
         racket/match)

(provide board%
         board-draw
         board-make-cell-pic
         board-cell-pic
         board-draw-cell-pic
         board-set-palette
         red-palette
         no-pen
         black-pen
         blue-pen
         red-pen
         no-brush
         board-ref
         board-set!
         board-ref-canvas
         board-get-matrix
         board-set-matrix
         )

#| MVC

To make a real separation between graphics and model,
the initialization should take a matrix as argument.
-> init the matrix and the gui separately.

But we can provide one in case none is provided?

|#



(define no-pen     (make-object pen% "BLACK" 1 'transparent))
(define black-pen  (make-object pen% "BLACK" 1 'solid))
(define red-pen    (make-object pen% "RED" 1 'solid))
(define blue-pen   (make-object pen% "BLUE" 1 'solid))

(define no-brush (make-object brush% "white" 'transparent))
;(define blue-brush (make-object brush% "blue" 'solid))

(define board%
  (class canvas%
    (init-field
     [mat #f]
     [num-cell-x #f]
     [num-cell-y num-cell-x] 
     [cell-dx 1] [cell-dy cell-dx]
     [inter-cell-dx 0] [inter-cell-dy inter-cell-dx]
     [on-mouse-event #f] ; either use this one, or some of the following:
     [on-left-down  (λ(xc yc)#f)]
     [on-right-down (λ(xc yc)#f)]
     [on-left-up    (λ(xc yc)#f)]
     [on-right-up   (λ(xc yc)#f)]
     [on-mouse-move (λ(x y)#f)]
     [(on-char-ext on-char) (λ(ch)#f)]
     )
    (init [style '()]
          )

    (unless mat
      (unless num-cell-x
        (error "Either mat or num-cell-x must be set."))
      (set! mat (make-matrix num-cell-y num-cell-x))) ; rows and cols are reversed!

    (define cell-size-x (+ cell-dx inter-cell-dx))
    (define cell-size-y (+ cell-dy inter-cell-dy))
    (define total-size-x 0);(* num-cell-x cell-size-x))
    (define total-size-y 0);(* num-cell-y cell-size-y))

    (super-new [min-width total-size-x]
               [min-height total-size-y]
               [style (append style '(no-autoclear))]
               [paint-callback 
                (λ(canvas dc)(send (super get-dc) draw-bitmap off-bitmap 0 0)
                  )])
        
    (set-matrix mat)

    (inherit refresh min-width min-height)
    
    (define/public (get-matrix) mat)
    (define/public (get-matrix-as-vector) (matrix-as-vector mat))
    ;; Modifies the current matrix.
    ;; If the sizes have changed, also change the corresponding properties
    (define/public (set-matrix m) 
      (set! mat m)
      (set! num-cell-x (matrix-ncols mat))
      (set! num-cell-y (matrix-nrows mat))
      (set! total-size-x (- (* num-cell-x cell-size-x) inter-cell-dx))
      (set! total-size-y (- (* num-cell-y cell-size-y) inter-cell-dy))
      (min-width total-size-x)
      (min-height total-size-y)
      )
    
    (define/public (get-total-size-x) total-size-x)
    (define/public (get-total-size-y) total-size-y)
    
    ;; A canvas coord to a matrix/cells coord
    (define/public (x->xc x)
      (inexact->exact (floor (/ x cell-size-x))))
    (define/public (y->yc y)
      (inexact->exact (floor (/ y cell-size-y))))
    
    ;; Cell coords to canvas coords
    (define/public (xc->x xc)
      (* xc cell-size-x))
    (define/public (yc->y yc)
      (* yc cell-size-y))
    
    (define/public (make-cell-pic color) ;:-> bitmap%
      ;: Creates a $bitmap% of the given color that as the correct sizes
      ;: for the current board.
      ;: Should be called only once for each different cell type,
      ;: i.e. at initialization time, not at runtime.
      (let* ([bm (make-object bitmap% cell-dx cell-dy)]
             [bm-dc (new bitmap-dc% [bitmap bm])])
        (send bm-dc set-pen no-pen)
        (send bm-dc set-brush (make-object brush% color 'solid))
        (send bm-dc draw-rectangle 0 0 cell-dx cell-dy)
        (send bm-dc set-bitmap #f)
        bm))
    (define cell-pic 
      (if (and (> cell-dx 0) (> cell-dy 0))
          (let ([c (make-cell-pic "blue")]) (λ(v)c))
          (λ(v)(make-object bitmap% 1 1)) ; ?????? how to make a "null" image ??????
          ;  (make-object bitmap% 0 0) is not possible.
          ))
      
    ; v is the value of the cell (to conditionnally choose an cell picture)
    (define/public (set-cell-pic proc)(set! cell-pic proc))
                         
    (define off-bitmap (make-object bitmap% total-size-x total-size-y))
    (define off-bitmap-dc (new bitmap-dc% [bitmap off-bitmap]))
    (define/override (get-dc) off-bitmap-dc)
    
    (define/public (draw-cell-pict x y bm)
      (send off-bitmap-dc draw-bitmap bm
            (* x cell-size-x)
            (* y cell-size-y)))
  
    (define/public (draw-background) ; call this at each run
      (send off-bitmap-dc clear)
      (matrix-for-each 
       mat 
       (λ(i j v)
         (let ([pic (cell-pic i j v)])
           (unless (symbol? pic)
             ; C'est ca qui prend du temps !!
             (send off-bitmap-dc draw-bitmap pic
                   (* j cell-size-x)
                   (* i cell-size-y))))
         ;(void)
         )))
    
    (define saved-background #f)
    
    ;; Call this function for general rendering of the board
    ;; Draw-other: use this to draw other things on the board than the cells
    ;; Should take another parameter for drawing the cells?
    ;; (instead of putting it somewhere weird?)
    (define/public (draw [draw-other (λ(dc)(void))])
      (draw-background)
      (draw-other (get-dc))
      (refresh)
      (set! saved-background #f)
      )
    
    
    ;; does not modify the background image, 
    ;; just draws something over it
    (define/public (draw-over proc)
      (unless saved-background
        ; if the background has changed since last time, re-save it
        ;(printf "re-save background\n")
        (set! saved-background (make-object bitmap% total-size-x total-size-y))
        (let ([saved-dc (make-object bitmap-dc% saved-background)])
          (send saved-dc draw-bitmap off-bitmap 0 0)
          (send saved-dc set-bitmap #f)
          ))
      (send off-bitmap-dc draw-bitmap
            saved-background 0 0)
      (proc (get-dc))
      (refresh)
      )
    
    (define/override (on-event evt)
      (let* ([x (send evt get-x)]
             [y (send evt get-y)]
             [xc (x->xc x)];(quotient x cell-size-x)]
             [yc (y->yc y)];(quotient y cell-size-y)]
             [evt-type (send evt get-event-type)]
             )
        (when (and (>= xc 0) (< xc num-cell-x) (>= yc 0) (< yc num-cell-y))
          (if on-mouse-event
              (on-mouse-event evt-type xc yc)
              (match evt-type
                ['left-down   (on-left-down xc yc)]
                ['right-down  (on-right-down xc yc)]
                ;['middle-down (on-middle-down xc yc)]
                ['left-up     (on-left-up xc yc)]
                ['right-up    (on-right-up xc yc)]
                ;['middle-up   (on-middle-up xc yc)]
                ['motion      (on-mouse-move x y)]
                ;['enter       (on-mouse-enter x y)]
                ;['leave       (on-mouse-leave x y)]
                [else (void)]
                )))
        ))
    
    (define/override (on-char ch)
      (on-char-ext ch))
    
    ))

(define-syntax-rule (board-draw board ms body ...)
  (begin 
    ; condition d'arrêt ?
    (send board draw-background)
    body ...  
    (send board refresh)
    (sleep/yield ms)
    ))

(define (board-cell-pic board proc)
  (send board set-cell-pic proc))

(define (board-set-palette board pal)
  (let ([pal
         (build-vector 
          (vector-length pal)
          (λ(n)(send board make-cell-pic 
                     (apply make-object color% (vector-ref pal n)))))])
    (send board set-cell-pic
          (λ(i j v)(vector-ref pal v)))))
; ex: (board-set-palette board red-palette)

(define red-palette
  (build-vector 256 (λ(n)(list n 0 0))))


(define (board-make-cell-pic board color)
  (send board make-cell-pic color))

(define (board-ref board x y)
  (matrix-ref (send board get-matrix) y x))

(define (board-ref-canvas board x y)
  (matrix-ref (send board get-matrix) 
              (send board y->cy y) 
              (send board x->cx x)))

(define (board-set! board x y v)
  (matrix-set! (send board get-matrix) y x v))

(define (board-draw-cell-pic board x y pic)
  (send board draw-cell-pict x y pic))

(define (board-get-matrix board)
  (send board get-matrix))

(define (board-set-matrix board mat)
  (send board set-matrix mat))

; une board-loop
; qui commence par effacer le board
; affcihe les cases
; laisse à l'utilisateur d'ajouter des choses (images) par dessus
; puis copie le bitmap à l'écran

;;; TEST:
(module+ main
  (define frame1 (new frame% [label "PLop"]))
  (define board1 (new board%
                      [parent frame1]
                      [num-cell-x 10] 
                      [cell-dx 48] 
                      [inter-cell-dx 1]))
  (send board1 set-cell-pic 
        (let([red (send board1 make-cell-pic "red")]
             [blue (send board1 make-cell-pic "blue")]) 
          (λ(i j v)(if (= 1 v) red blue))));'none))))
  
  (send frame1 show #t)
  (send board1 draw)
  ;
  ;;(sleep/yield 1)
  ;(define mat1 (send board1 get-matrix))
  ;
  ;(time
  ;(let loop ([i 0])
  ;  (when (< i 100)
  ;    (matrix-map! mat1 (λ(i j v)(random 2)))
  ;    (send board1 on-paint)
  ;;    (send board1 refresh)
  ;    (sleep/yield 0.01)
  ;    (loop (+ i 1)))))
  )
