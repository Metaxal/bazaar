#lang racket/base
;;; Copyright (C) Laurent Orseau, 2010-2013
;;; GNU Lesser General Public Licence (http://www.gnu.org/licenses/lgpl.html)

(provide (all-defined-out))

;; Returns the number of years, days, minutes and seconds as a list,
;; when given a number of seconds.
;; If annotated is not #f, the list contains symbol annotations for readablity.
(define (seconds->duration secs #:annotated? [annotated? #f])
    (define t secs)
    (define s (modulo t 60))
    (set! t (quotient t 60))
    (define m (modulo t 60))
    (set! t (quotient t 60))
    (define d (modulo t 24))
    (define a (quotient t 24))
    (if annotated?
        (list a 'years d 'days m 'minutes s 'seconds)
        (list a d m s)))
