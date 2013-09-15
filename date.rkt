#lang racket/base
;;; Copyright (C) Laurent Orseau, 2010-2013
;;; GNU Lesser General Public Licence (http://www.gnu.org/licenses/lgpl.html)

(require racket/string
         racket/format
         racket/date)

(provide (all-defined-out)
         (all-from-out racket/date))

;; Returns the string of the current date with the specified date-display-format.
;; Just saves the use of a parameterize and date->string and (current-date)
(define (current-date-string [time #t] [format (date-display-format)]
                             #:date [d (current-date)])
  (parameterize ([date-display-format format])
    (date->string d time)))

(define (date-rfc [time #t] #:date [d (current-date)])
  (current-date-string time 'rfc2822))

(define (date-iso [time #t] #:date [d (current-date)])
  (current-date-string time 'iso-8601))

;; Returns the date string with the numbers in iso order (lexicographical order)
;; Separators can be modified.
;; Can print the year-month-day alone, the time alone or both
(define (date-iso-like [d (current-date)]
                       #:time? [time? #t]
                       #:seconds? [seconds? #t]
                       #:year-month-day? [ymd? #t]
                       #:date-sep [dsep "-"]
                       #:date-time-sep [dtsep " "]
                       #:time-sep [tsep ":"])
  (define (fmt field [len 2])
    (~r (field d) #:min-width len #:pad-string "0"))
  (string-append
   (if ymd?
       (string-append
        (fmt date-year 4)
        dsep
        (fmt date-month)
        dsep
        (fmt date-day))
       "")
   (if time?
       (string-append
        (if ymd? dtsep "")
        (fmt date-hour)
        tsep
        (fmt date-minute)
        (if seconds?
            (string-append
             tsep
             (fmt date-second))
            ""))
       "")))

;; Iso date-time format suitable for filenames (cross-platform)
(define (date-iso-file [d (current-date)] #:time? [time? #t])
  (date-iso-like d #:time? time?
                 #:date-sep "-"
                 #:time-sep "-"
                 #:date-time-sep "--"))

(module+ test
  (require rackunit)
  
  (define d (date* 3 4 5 1 2 2013 3 232 #t 7200 43508052 "CEST"))
  
  (check-equal? (date-iso-like d)
                "2013-02-01 05:04:03")
  (check-equal? (date-iso-like d #:time? #f)
                "2013-02-01")
  (check-equal? (date-iso-like d #:year-month-day? #f)
                "05:04:03")
  (check-equal? (date-iso-like d #:seconds? #f)
                "2013-02-01 05:04")
  
  (check-equal? (date-iso-like d #:seconds? #f
                               #:date-sep "/"
                               #:time-sep "h")
                "2013/02/01 05h04")
  
  (check-equal? (date-iso-file d)
                "2013-02-01--05-04-03")
  
  )

