#lang racket/base

(require define2
         racket/match)

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
  (define h (modulo t 24))
  (set! t (quotient t 24))
  (define d (modulo t 365))
  (define y (quotient t 365))
  (case annotation
    [(long) (list y 'years d 'days h 'hours m 'minutes s 'seconds ms 'milliseconds)]
    [(short) (list y 'y d 'd h 'h m 'm s 's ms 'ms)]
    [else (list y d h m s ms)]))

(define (duration->milliseconds l)
  (match-define (list y d h m s ms) l)
  (+ ms (* 1000 (+ s (* 60 (+ m (* 60 (+ h (* 24 (+ d (* y 365)))))))))))

(module+ test
  (require rackunit)
  (define dur (list (random 1000) (random 365) (random 24) (random 60) (random 60) (random 1000)))
  (for ([i (in-range 100)])
    (check-equal? (milliseconds->duration (duration->milliseconds dur))
                  dur)))

