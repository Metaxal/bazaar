#lang racket/base
(require racket/string
         racket/list)

(provide (all-defined-out))

(define (make-px l)
  (pregexp
   (string-append*
    (for/list ([x (in-list l)])
      (if (list? x)
          (string-append "(" (first x) ")")
          (regexp-quote x))))))

;; Usage example:
#;
(filter
 values
 (for/list ([str (file->lines file)])
   (match str
     [(pregexp
       (make-px
        '[(".*" #;"DOVIBEX")
          ": "
          ("[\\d.]*" #;"418.22") "s elapsed; "
          ("\\d*" #;"2025247213") " expanded; "
          ("\\d*" #;"6014801337") " generated; solution length "
          ("[\\d.]*" #;"66.305803")])
       (list _ alg elapsed expanded generated sol-length))
      (set! elapsed (string->number elapsed))
      (set! expanded (string->number expanded))
      (set! generated (string->number generated))
      (set! sol-length (string->number sol-length))
      (vars->assoc alg elapsed expanded generated sol-length)]
     [else (error str)])))

