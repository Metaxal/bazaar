#lang racket/base

;;; Copyright (C) Laurent Orseau, 2010
;;; GNU General Public Licence 3 (http://www.gnu.org/licenses/)

(provide (all-defined-out))

(require racket/list)

(define (version-string->number str)
  (map string->number (regexp-split "\\." str)))

(define (version-number->string l)
  (apply string-append (add-between (map number->string l) ".")))

(define (version<? v1 v2)
  (let loop ([vl1 (version-string->number v1)]
             [vl2 (version-string->number v2)])
    (cond [(and (empty? vl1) (empty? vl2)) #f]
          [(empty? vl1) #t]
          [(empty? vl2) #f]
          [(< (first vl1) (first vl2)) #t]
          [(> (first vl1) (first vl2)) #f]
          [else (loop (rest vl1) (rest vl2))])))

#| Examples
> (version<? "1.04.5" "1.04.6")
#t
> (version<? "1.04.05" "1.04.6")
#t
> (version<? "1.04.05" "1.04.4")
#f
> (version<? "1.05.05" "1.04.6")
#f
> (version<? "1.05.05" "0.04.6")
#f
> (version<? "1.05.05" "2.04.6")
#t
|#
