#lang racket/base

(require net/url
         racket/contract
         racket/string
         racket/port)

(provide (contract-out [url-contents ((or/c url? string?) . -> . string?)]
                       [url-bust-cache (url? . -> . url?)]))

;; Sometimes an intermediate node may still serve a cached version despite
;; headers. To prevent this, we simply add some noise in the url.
(define (url-bust-cache aurl)
  (struct-copy url aurl
               [query (cons (cons 'cachebust (number->string (current-milliseconds)))
                            (url-query aurl))]))

;; Accepts a string representing a URL and returns the corresponding raw webpage.
(define (url-contents url/str #:bust-cache? [bust-cache? #false])
  (let ([aurl (if (url? url/str)
                url/str
                (string->url url/str))])
    (port->string (get-pure-port (if bust-cache? (url-bust-cache aurl) aurl)
                                 #:redirections 10)
                  #:close? #true)))

