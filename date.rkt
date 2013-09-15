#lang racket/base
;;; Copyright (C) Laurent Orseau, 2010-2013
;;; GNU Lesser General Public Licence (http://www.gnu.org/licenses/lgpl.html)

(require racket/format
         racket/date)

(provide (all-defined-out)
         (all-from-out racket/date))

;; Returns the string of the current date with the specified date-display-format.
;; Just saves the use of a parameterize and date->string and (current-date)
(define (date->format [d (current-date)] [format (date-display-format)]
                              #:time? [time? #t])
  (parameterize ([date-display-format format])
    (date->string d time?)))

(define (date-rfc [d (current-date)] #:time? [time? #t])
  (date->format d 'rfc2822 #:time? time?))

(define (date-iso [d (current-date)] #:time? [time? #t])
  (date->format d 'iso-8601 #:time? time?))

;; Returns the date string with the numbers in iso order (lexicographical order)
;; Separators can be modified.
;; Can print the year-month-day alone, the time alone or both.
;; The default format is like `date-iso' without the "T"
;; (the problem with the "T" is that it makes day and hours difficult to separate visually,
;; An underscore would probably have been a better separator for the iso norm...)
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
  
  (check-equal? (date-iso d)
                "2013-02-01T05:04:03")
  
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

