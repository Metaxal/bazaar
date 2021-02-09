#lang racket/base

;;; Notification box.
;;; Creates a frame that is always visible (under X: on each workspace),
;;; does not take keyboard focus, has no resize-border, no title,
;;; no system menu, no menu bar.

(require racket/gui/base racket/class)

(provide float-box%)

(define float-box%
  (class frame%
    (super-new [label ""]
               [style '(no-resize-border 
                        no-caption
                        no-system-menu
                        hide-menu-bar
                        float)])
    
    (init-field message
                [icon #f] ; path
                )
    (init [style           '(ok)]
          [ok-callback     (λ() (void))]
          [cancel-callback (λ() (void))]
          )
    
    (define hp (new horizontal-panel% [parent this]
                    [alignment '(center center)]))
    (when icon
      (new message% [parent hp] [label icon]))
    (define msg (new message% [parent hp] [label message]))
    
    (define hp-buttons (new horizontal-panel% [parent this] 
                            [alignment '(center center)]))
    
    (define bt-ok #f)
    (define bt-cancel #f)
    (when (or (member 'ok style) (member 'ok-cancel style))
      (set! bt-ok (new button% [parent hp-buttons] 
                       [label "Ok"]
                       [min-width 120]
                       [callback (λ(bt evt)(send this show #f)
                                   (ok-callback))])))

    (when (member 'ok-cancel style)
      (set! bt-cancel (new button% [parent hp-buttons] 
                           [label "Cancel"]
                           [min-width 120]
                           [callback (λ(bt evt)(send this show #f)
                                       (cancel-callback))])))
    

    ; Center me on the screen
    (send this reflow-container)
    (let-values ([(fbw fbh) (send this get-size)]
                 [(scw sch) (get-display-size)])
      (send this move (quotient (- scw fbw) 2) (quotient (- sch fbh) 2)))
    
    ))


(module+ main
  (require images/icons/symbol)
  (define fb (new float-box% [message "You will be recycled."]
                  [icon (recycle-icon #:height 80)]
                  [style '(ok-cancel)]
                  ;[min-width 660]
                  ))
  
  (send fb show #t)
  )
