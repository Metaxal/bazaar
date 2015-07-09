#lang racket/gui

(require bazaar/gui/board)

;;; A small example demonstrating usage of the board module.

(define frame1 (new frame% [label "Board"]))
(define board1
  (new board%
       [parent frame1]
       [num-cell-x 10] ; number of cells on the board
       [cell-dx 48] ; size of a cell
       [inter-cell-dx 1] ; pixels between two cells (can be 0)
       [on-left-down ; change color of current cell when left button pressed
        (λ(xc yc)
          (board-update! board1 xc yc (λ(v)(- 1 v)))
          (send board1 draw)
          (sleep/yield 0.1))]))

(send board1 set-cell-pic 
      ; create pictures the size of a cell of a given color
      (let([red   (send board1 make-cell-pic "red")]
           [blue  (send board1 make-cell-pic "blue")]) 
        ; decide what cells to show at place (i, j) of value v
        (λ(i j v)
          (case v
            [(0)  blue]
            [else red]))))

(send frame1 show #t)
(send board1 draw)
(sleep/yield 0.1)
