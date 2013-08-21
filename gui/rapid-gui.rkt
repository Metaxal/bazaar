#lang racket/base
;;; Copyright (C) Laurent Orseau, 2010-2013
;;; GNU Lesser General Public Licence (http://www.gnu.org/licenses/lgpl.html)

(require racket/gui/base
         racket/class
         (for-syntax syntax/parse racket/base))

;;;    *********************    ;;;
;;;    ***   Rapid Gui   ***    ;;;
;;;    *********************    ;;;

(provide rapid-gui)
(define-syntax-rule (rapid-gui obj)
  (rapid-gui-parent #f obj))

(define-syntax (rapid-gui-parent stx)
  (syntax-case stx () ; todo: use syntax-parser
    [(_ parent (cl-name (args ...) child ...))
     (with-syntax ([name (gensym (syntax-e #'cl-name))])
       #'(rapid-gui-named parent cl-name name (args ...) child ...))
     ]
    [(_ parent (cl-name name (args ...) child ...))
     #'(rapid-gui-named parent cl-name name (args ...) child ...)
     ]))

(define-syntax-rule (rapid-gui-named par cl-name name (args ...) child ...)
  (begin
      (define name (new cl-name [parent par] args ...))
      (rapid-gui-parent name child) ...))

;=============;
;=== Tests ===;
;=============;
(module+ main
  
  (rapid-gui
   (frame% 
    fr1 ([label "a frame"] [min-width 400])
    (horizontal-panel%
     ([min-width 400])
     (button% bt1 ([label "Button 1"]))
     (button% bt2 ([label "Close"]
                   [callback (λ(bt ev)(send fr1 show #f))]))
     )
    (horizontal-panel%
     ([min-width 400])
     (check-box% cb1 ([label "CB 1"]))
     (check-box% cb2 ([label "CB 2"]))
     )
    (message% ([label "Hello!"]))
    ))
  (send fr1 show #t)
  (send cb1 get-value)
  )