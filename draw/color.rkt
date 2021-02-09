#lang racket/base

(require racket/draw
         racket/class)

(provide (all-defined-out))

(define (color/string->color c)
  (cond [(is-a? c color%) c]
        [(string? c)
         (send the-color-database find-color c)]
        [else (error "Unknown color type" c)]))

;; r, g, b: (real-in 0.0 1.0)
;; -> color%
(define (rgb->color r g b)
  (make-object color%
    (inexact->exact (floor (* r 255)))
    (inexact->exact (floor (* g 255)))
    (inexact->exact (floor (* b 255)))))

;; Returns the list of the [0-255] hue values
(define (color->rgb-list c)
  (list (send c red)(send c green)(send c blue)))
