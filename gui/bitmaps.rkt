#lang racket/base

(require racket/gui/base
         racket/class
         "../define.rkt"
         )
;(provide (all-defined-out))

(define white (make-object color% 255 255 255))

(define/provide (draw-bitmap/mask dc bmp x y [mask (send bmp get-loaded-mask)])
  (let-values ([(w h) (send dc get-size)]
               [(wb hb) (values (send bmp get-width) (send bmp get-height))])
    (send dc draw-bitmap
          bmp 
          (cond [(number? x) x]
                [(equal? x 'center) (/ (- w wb) 2)]
                [(equal? x 'right)  (- w wb)]
                [(equal? x 'left)   0])
          (cond [(number? y) y]
                [(equal? y 'center) (/ (- h hb) 2)]
                [(equal? y 'bottom) (- h hb)]
                [(equal? y 'top)    0])
          'solid white
          mask
          )))

;; proc: (λ (a r g b x y) ) -> (values a r g b)
;; mask: (or 'auto #f bitmap%)
(define/provide (bitmap-map! proc bmp [mask #f])
  (let* ([w (send bmp get-width)]
         [h (send bmp get-height)]
         [pixels (make-bytes (* w h 4))]
         [mask (if (equal? mask 'auto) (send bmp get-loaded-mask) mask)]
         [dc (make-object bitmap-dc% bmp)]
         [mask-dc (and mask (make-object bitmap-dc% mask))]
         )
    (send bmp get-argb-pixels 0 0 w h pixels)
    (when mask (send mask-dc get-argb-pixels 0 0 w h pixels #t))
    (for* ([i (in-range w)]
           [j (in-range h)])
      (let* ([p (* (+ i (* j w)) 4)]
             [a (bytes-ref pixels (+ p 0))]
             [r (bytes-ref pixels (+ p 1))]
             [g (bytes-ref pixels (+ p 2))]
             [b (bytes-ref pixels (+ p 3))])
        (let-values ([(a r g b) (proc a r g b i j)])
          (bytes-set! pixels (+ p 0) a)
          (bytes-set! pixels (+ p 1) r)
          (bytes-set! pixels (+ p 2) g)
          (bytes-set! pixels (+ p 3) b)
          )))
    (send dc set-argb-pixels 0 0 w h pixels)
    (send dc set-bitmap #f)
    (when mask 
      (send mask-dc set-argb-pixels 0 0 w h pixels #t)
      (send mask-dc set-bitmap #f))
;    (make-object bitmap% pixels w h)
    ))

(define/provide (copy-bitmap bmp)
  (let* ([w (send bmp get-width)]
         [h (send bmp get-height)]
         [bmp2 (make-object bitmap% w h)]
         [dc (make-object bitmap-dc% bmp2)]
         )
    (send dc draw-bitmap bmp 0 0)
    (send dc set-bitmap #f)
    (send bmp2 set-loaded-mask (send bmp get-loaded-mask))
    ; return:
    bmp2
    ))

(define/provide (bitmap-map proc bmp)
  (let ([bmp2 (copy-bitmap bmp)])
    (bitmap-map! proc bmp2 'auto)
    bmp2))
  
#| Tests | #

;; Useful for quickly displaying bitmaps (for tests)
(define show-bmp-x 100)
(define show-bmp-y 100)
(define (show-bitmap title bmp [background "white"])
  (let ([frame (new frame% [label title]
                    [x show-bmp-x]
                    [y show-bmp-y]
                    )]
        [bkg-color (if (string? background)
                       (send the-color-database find-color background)
                       background)]
        [w (max 120 (send bmp get-width))]
        )
    (set! show-bmp-x (+ show-bmp-x w 10))
    ;(set! show-bmp-y (+ show-bmp-y (send bmp get-height) 10))
    (new canvas% [parent frame]
         [min-width w]
         [min-height (send bmp get-height)]
         [paint-callback
          (λ (cv dc)
            (send dc set-background bkg-color)
            (send dc clear)
            (send dc draw-bitmap bmp 0 0
                  'solid bkg-color
                  (send bmp get-loaded-mask)
                  )
            )]
         )
    (send frame show #t)
    ))

;; TESTS:

(define bmp 
  (make-object bitmap% "player-two.png" 'png/mask))

(define bmp2  
  (bitmap-map 
   ; (λ (a r g b) (values (min a 120) g b r)) 
   (λ (a r g b x y) (values a g g (quotient g 8)))
   bmp);bmp-mask)
  )

(show-bitmap "bmp" bmp "darkblue")
(show-bitmap "bmp" bmp "lightgreen")
(show-bitmap "bmp2" bmp2 "darkblue")
(show-bitmap "bmp2" bmp2 "lightgreen")
(show-bitmap "trucx" bmp)

;(send bmp2 save-file "player-two.png" 'png)


;|#