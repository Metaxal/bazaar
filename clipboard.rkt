#lang racket/base

(require racket/gui/base
         racket/format
         racket/class)

(provide clip clipx)

(define clip
  (case-lambda
    [() (send the-clipboard get-clipboard-string 0)]
    [elts (send the-clipboard set-clipboard-string (apply ~a elts) 0)]))

(define clipx
  (case-lambda
    [() (send the-x-selection-clipboard get-clipboard-string 0)]
    [elts (send the-x-selection-clipboard set-clipboard-string (apply ~a elts) 0)]))

(module+ test
  (require rackunit)
  (clip "bépo½")
  (check-equal? (clip)
                "bépo½")

  ;; clipx doesn't seem to work, even though (or because?)
  #;(require racket/file)
  #;(get-preference 'GRacket:selectionAsClipboard)
  ; > #f

  )
