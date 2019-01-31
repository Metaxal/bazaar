#lang racket/base
;;; Copyright (C) Laurent Orseau, 2010-2013
;;; GNU Lesser General Public Licence (http://www.gnu.org/licenses/lgpl.html)

(require racket/match)

(provide (all-defined-out))

;; Returns the number of years, days, minutes and seconds as a list,
;; when given a number of seconds.
;; If annotation is not #f, the list contains symbol annotations for readablity.
(define (milliseconds->duration secs #:annotation [annotation #f])
  (define t secs)
  (define ms (modulo t 1000))
  (set! t (quotient t 1000))
  (define s (modulo t 60))
  (set! t (quotient t 60))
  (define m (modulo t 60))
  (set! t (quotient t 60))
  (define d (modulo t 24))
  (define a (quotient t 24))
  (case annotation
    [(long) (list a 'years d 'days m 'minutes s 'seconds ms 'milliseconds)]
    [(short) (list a 'y d 'd m 'm s 's ms 'ms)]
    [else (list a d m s ms)]))

(define (duration->milliseconds l)
  (match-define (list a d m s ms) l)
  (+ ms (* 1000 (+ s (* 60 (+ m (* 60 (+ d (* 24 a)))))))))

(module+ test
  (require rackunit)
  (define dur (list (random 1000) (random 24) (random 60) (random 60) (random 1000)))
  (for ([i (in-range 100)])
    (check-equal? (milliseconds->duration (duration->milliseconds dur))
                  dur)))

