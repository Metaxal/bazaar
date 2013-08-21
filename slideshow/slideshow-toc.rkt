#lang slideshow
;;; Copyright (C) Laurent Orseau, 2010-2013
;;; GNU Lesser General Public Licence (http://www.gnu.org/licenses/lgpl.html)

(require "../mutation.rkt")
         
(provide build-table-of-contents
         section/slide
         subsection/slide
         current-section-num
         current-subsection-num
         current-slide-section
         current-slide-subsection
         )

#;(define-syntax-rule (++ var)
  (set! var (add1 var)))

;; Apparently, this is needed.
;; It seems that the ++ operation is done in another module due to the macro,
;; and it then considers that it is not mutable.
(define (++current-section-num)
  (++ current-section-num))
(define (++current-subsection-num)
  (++ current-subsection-num))
(define (init-current-subsection-num)
  (set! current-subsection-num -1))


(define-syntax-rule (build-table-of-contents get-table-of-contents)
  (define-syntax (get-table-of-contents stx)
    (with-syntax ([parts table-of-contents])
      #'(reverse (map reverse 'parts)))))

(define current-slide-section (make-parameter #f))
(define current-slide-subsection (make-parameter #f))
(define current-section-num -1)
(define current-subsection-num -1)
(define-for-syntax table-of-contents '())

(define-syntax (section/slide stx)
  (syntax-case stx ()
    [(_ title e ...)
     (with-syntax ([title #'title])
       (set! table-of-contents (cons (list (syntax->datum #'title)) table-of-contents))
       ;(print parts)(newline)
       #'(begin
           (++current-section-num)
           (init-current-subsection-num)
           (when (current-slide-section)
             ((current-slide-section) title e ...))
           ))]))

(define-syntax (subsection/slide stx)
  (syntax-case stx ()
    [(_ title e ...)
     (with-syntax ([title #'title])
       (when (null? table-of-contents)
         (raise-syntax-error 'slide-subsection "Subsection before any section"))
       (define section (car table-of-contents))
       (set! table-of-contents 
             (cons (cons (syntax->datum #'title) section)
                   (cdr table-of-contents)))
       ;(print parts)(newline)
       #'(begin
           (++current-subsection-num)
           (when (current-slide-subsection)
             ((current-slide-subsection) title e ...))
           ))]))
