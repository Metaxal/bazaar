#lang racket/base
;;; Copyright (C) Laurent Orseau, 2010-2013
;;; GNU Lesser General Public Licence (http://www.gnu.org/licenses/lgpl.html)

(require racket/string
         racket/format
         racket/date)

(provide (all-defined-out)
         (all-from-out racket/date))

(define (current-date-string [time #t] [format (date-display-format)]
                             #:date [d (current-date)])
  (parameterize ([date-display-format format])
    (date->string d time)))

(define (date-rfc [time #t] #:date [d (current-date)])
  (current-date-string time 'rfc2822))

(define (date-iso [time #t] #:date [d (current-date)])
  (current-date-string time 'iso-8601))

;; Iso date-time format suitable for filenames
(define (date-iso-file [time #t] #:date [d (current-date)])
  (define (fmt field [len 2])
    (~r (field d) #:min-width len #:pad-string "0"))
  (string-append
   (format "~a-~a-~a"
           (fmt date-year)
           (fmt date-month)
           (fmt date-day))
   (if time 
       (format "--~a-~a-~a"
               (fmt date-hour)
               (fmt date-minute)
               (fmt date-second))
       "")))
  #;(string-replace (string-replace (date-iso time) ":" "-") "T" "--")

(module+ test
  (require rackunit)
  
  (check-equal? 
   (date-iso-file #:date (date* 3 4 5 1 2 2013 3 232 #t 7200 43508052 "CEST"))
   "2013-02-01--05-04-03"))