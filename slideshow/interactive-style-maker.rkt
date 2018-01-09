#lang racket/base
;;; Copyright (C) Laurent Orseau, 2010-2013
;;; GNU Lesser General Public Licence (http://www.gnu.org/licenses/lgpl.html)

(require racket/gui/base
         "warsaw.rkt"
         "slideshow-utils.rkt"
         "../draw/color.rkt"
         )

(provide interactive-style-maker)

(define (pick-color [color #f])
  (or (get-color-from-user #f #f color) color))

;(define (pick-rgb [color #f])
;  (color->rgb-list (pick-color color)))

(define (pick-font old-face old-size)
  (let ([f (get-font-from-user #f #f (find-font old-face old-size))])
    (if f (values (send f get-face) (send f get-point-size))
        (values old-face old-size))))

(define (pick-text message old-txt)
  (or (get-text-from-user "Enter new text" message #f old-txt) old-txt))

;; Constructor-write
;; Writes a value so that evaluating it generates an equal? value
(define space (gensym))
(define return (gensym))
(define (ctr-write x)
  (cond [(is-a? x color%) (write `(make-object color% ,@(color->rgb-list x)))]
        [(equal? x space) (display " ")]
        [(equal? x return) (newline)]
        [(and (list? x)(not (empty? x))(equal? 'quote (first x)))
         (display "'")(ctr-write (second x))]
        [(list? x) (display "(")
                   (for-each ctr-write (add-between x space)) 
                   (display ")")]
        [else (write x)]))

(define (ctr-writeln x)
  (ctr-write x)(newline))

(define (find-font face/family size)
  (send the-font-list find-or-create-font size
        (if (string? face/family) face/family #f)
        (if (symbol? face/family) face/family 'default)
        'normal
        'normal))

;; bullet-assoc: (name . (pic code))
;; by default, name and pic are the same.
(define-syntax-rule (make-bullet-assoc bul ...)
  (list (list bul bul 'bul) ...))
(define bullet-assoc 
  (make-bullet-assoc 
   bullet o-bullet
   red-sphere-bullet green-sphere-bullet blue-sphere-bullet 
   cyan-sphere-bullet yellow-sphere-bullet magenta-sphere-bullet
   black-sphere-bullet gray-sphere-bullet
   ))

(define (interactive-style-maker)
  (let ([title "Title"]
        [auths "Authors"]
        [left-color  (current-left-color)]
        [right-color (current-right-color)]
        [bk-color    (current-background-color)]
        [font-size   (current-font-size)]
        [main-font   (current-main-font)]
        [title-color (current-title-color)]
        [body-color  (current-body-color)]
        [ibul        (current-item-bullet)] ; NAME of the bullet 
        ; (= pic most of the time)
        ; except for color-bullet and colorsubbullet, see below
        [subibul     (current-subitem-bullet)]
        [aclickback  (λ(pic thunk)
                       (clickback (if (string? pic)
                                      (text-button pic)
                                      pic)
                                  thunk))]
        )
    (let loop ()            
      (current-main-font   main-font)
      (current-font-size   font-size)
      (current-title-color title-color)
      (current-body-color  body-color)
      (current-left-color  left-color)
      (current-right-color right-color)
      (current-background-color bk-color)
      (init-slide-style title auths)
      (let ([lclickback (λ(pic thunk)(aclickback pic (λ _ (thunk) (loop))))]
            ; add bullets that are specific to the current style:
            ; their name is not the picture, because the picture changes,
            ; and not the name
            [bullet-assoc (append bullet-assoc
                                  `((color-bullet
                                     ,(color-bullet right-color)
                                     'color-bullet)
                                    (color-subbullet
                                     ,(color-subbullet right-color bk-color)
                                     'color-subbullet)))]
            )
        (current-item-bullet    (first (dict-ref bullet-assoc ibul)))
        (current-subitem-bullet (first (dict-ref bullet-assoc subibul)))
        
        (retract-most-recent-slide)
        (slide 
         #:title "Slide title"
         (item 
           (t "Change colors"))
         (smaller 
          (subitem 
           (lclickback "Title"
                       (λ _ (set! title-color (pick-color title-color))))
           (lclickback "Body"
                       (λ _ (set! body-color (pick-color body-color))))
           (lclickback "Left"
                       (λ _ (set! left-color (pick-color left-color))))
           (lclickback "Right"
                       (λ _ (set! right-color (pick-color right-color))))
           (lclickback "Swap left ↔ right"
                       (λ _ (set!-values (left-color right-color)
                                         (values right-color left-color))))
           (lclickback "Background"
                       (λ _ (set! bk-color (pick-color bk-color))))
           ))
         (item "Change text and style")
         (smaller
          (subitem
           (lclickback "Title"
                       (λ _ (set! title (pick-text "New title:" title))
                         (loop)))
           (lclickback "Authors"
                       (λ _ (set! auths (pick-text "New authors:" auths))))
           (lclickback "Font"
                       (λ _ (set!-values (main-font font-size)
                                         (pick-font main-font font-size))))))
         (item (apply hbl-append 10
                      (t "Select item bullet:")
                      (map (λ(b)(clickback (second b)
                                           (λ _ (set! ibul (first b)) (loop))))
                           bullet-assoc)))
         (subitem (apply hbl-append 10
                         (t "Select subitem bullet:")
                         (map (λ(b)(clickback (second b)
                                              (λ _ (set! subibul (first b)) (loop))))
                              bullet-assoc)))
         
         (aclickback "→ Generate code ←"
                     (λ _ 
                       (displayln "#lang racket/base
(require racket/draw/private/color
         bazaar/slideshow/warsaw) ; replaces slideshow
")
                       (for-each
                        ctr-writeln
                        `((current-main-font ,(if (string? main-font) 
                                                  main-font
                                                  `',main-font))
                          (current-font-size ,font-size)
                          (current-title-color ,title-color)
                          (current-body-color ,body-color)
                          (current-left-color ,left-color)
                          (current-right-color ,right-color)
                          (current-background-color ,bk-color)
                          (current-item-bullet ,(second (dict-ref bullet-assoc ibul)))
                          (current-subitem-bullet 
                           ,(second (dict-ref bullet-assoc subibul)))
                          ,space
                          (init-slide-style ,title ,auths)
                          ,space
                          (title-slide "Some Title"
                                       (t "The author")
                                       (t "..."))
                          (slide #:title "Slide title"
                                 (t "Some text")
                                 (item "An item...")
                                 (subitem "... and a subitem")
                                 (text-button "Some more text"))
                          )))))))))

(module+ main
  (interactive-style-maker))
