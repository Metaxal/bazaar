#lang racket/base

(require net/url
         racket/port
         racket/contract)

(provide (contract-out [url-contents ((or/c url? string?) . -> . string?)]))

;; Accepts a string representing a URL and returns the corresponding raw webpage.
(define (url-contents url/str)
  (port->string (get-pure-port (if (url? url/str)
                                   url/str
                                   (string->url url/str)))))

