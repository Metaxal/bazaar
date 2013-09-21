#lang racket/base
;;; Copyright (C) Laurent Orseau, 2010-2013
;;; GNU Lesser General Public Licence (http://www.gnu.org/licenses/lgpl.html)

(require net/smtp
         net/head
         racket/string
         racket/tcp)

(provide make-smtp-send)

(define (make-smtp-send smtp-server from 
                        #:port-no [port-no 25]
                        #:auth-user [auth-user #f]
                        #:auth-passwd [auth-passwd #f]
                        #:tcp-connect [tcp tcp-connect]
                        #:tls-encode [tls #f]
                        )
  ; to : (or/c string? (listof string?))
  ; subject: string?
  ; lines: (or/c string? (listof string?))
  (λ(to subject lines
        #:cc [cc '()] #:bcc [bcc '()]
        #:from [from from])
    (set! to (if (string? to) (list to) to))
    (set! lines (if (string? lines)
                    (string-split lines "\n")
                    lines))
    (smtp-send-message
     smtp-server
     from
     to
     (standard-message-header
      from to cc bcc
      subject)
     lines
     #:port-no port-no
     #:auth-user auth-user
     #:auth-passwd auth-passwd
     #:tcp-connect tcp
     #:tls-encode tls
     )))

#| Usage

(define my-send
  (make-smtp-send
   "smpt.my-server.com"
   "postmaster@my-server.com"
   #:auth-user "postmaster@my-server.com"
   #:auth-passwd "correct horse battery stapple"))

(my-send
 "you@some-server.com"
  "Long time no see"
 "Hey there!
Haven't seen you in a long time, I hope all is well.
What about diner tonight? Yes, like every night, but this time without cellphones!
See ya,
Your husband.
")

|#