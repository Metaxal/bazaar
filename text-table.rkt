#lang racket/base
;;; Copyright (C) Laurent Orseau, 2010-2013
;;; GNU Lesser General Public Licence (http://www.gnu.org/licenses/lgpl.html)

(require srfi/13
         racket/dict)

(provide (all-defined-out))

(define (->string a)
  (format "~a" a))

;;;;;;;;;;;;;;;;;;;;
;;; ASCII Tables ;;;
;;;;;;;;;;;;;;;;;;;;

;; head : (or '< '+ '> (col len))

(struct atable (head dash sep first-line mid-line last-line)
  #:transparent)

(define (table-map t f-col ch-deb ch ch-end)
  (let ([n 0])
    (map 
     (λ(s)
       (cond [(equal? s '<) ch-deb]
             [(equal? s '+) ch]
             [(equal? s '>) ch-end]
             [(list? s) (begin0 (apply f-col n s)
                                (set! n (+ 1 n)))]
             ;[(string? s) (f-col s (string-length s))]
             ))
     (atable-head t))))

(define (table-row t row)
    (apply string-append
           (table-map t 
                      (λ(n str len) 
                        (string-pad-right (->string (list-ref row n)) len))
                      (atable-sep t) (atable-sep t) (atable-sep t)
                      )))

(define (table-head t)
   (apply string-append
          (table-map t 
                     (λ(n str len)
                       (string-pad-right (->string str) len))
                     (atable-sep t) (atable-sep t) (atable-sep t))
          ))

(define (table-line t getter)
    (apply string-append
           (apply table-map t 
                  (λ(n str len) 
                    (build-string len (λ(n)(atable-dash t))))
                  (getter t)
                  )))
(define (table-first-line t)
  (table-line t atable-first-line))
(define (table-mid-line t)
  (table-line t atable-mid-line))
(define (table-last-line t)
  (table-line t atable-last-line))

;; borders: (or 'normal 'rounded 'double)
(define (table-framed head [borders 'rounded])
  (apply atable head
         (dict-ref 
          '((normal   . (#\─ "│" ("┌" "┬" "┐") ("├" "┼" "┤") ("└" "┴" "┘")))
            (rounded  . (#\─ "│" ("╭" "┬" "╮") ("├" "┼" "┤") ("╰" "┴" "╯")))
            (double   . (#\═ "║" ("╔" "╦" "╗") ("╠" "╬" "╣") ("╚" "╩" "╝"))))
          borders)))

(module+ main
  
  (define t1 (table-framed '(< ("i" 4) + + ("f1" 8) + ("f2" 4) >) 
                           'double
                           ;'normal
                           ;'rounded
                           ))
  (for-each displayln 
            (list (table-first-line t1)
                  (table-head t1)
                  (table-mid-line t1)
                  (table-mid-line t1)
                  (table-row t1 '(a b c))
                  (table-mid-line t1)
                  (table-row t1 '(x y "z"))
                  (table-last-line t1)))
  )
