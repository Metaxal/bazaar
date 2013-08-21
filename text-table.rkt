#lang racket/base

;;; Laurent Orseau <laurent orseau gmail com> -- 2010-02-10
;;; License: WTFPL

(require srfi/13
         racket/dict)

(provide (all-defined-out))

(define (->string a)
  (format "~a" a))

;;;;;;;;;;;;;;;;;;;;
;;; ASCII Tables ;;;
;;;;;;;;;;;;;;;;;;;;

;; head : (or '< '+ '> (col len))

(struct table (head dash sep first-line mid-line last-line)
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
     (table-head t))))

(define (print-table-row t row)
  (displayln
    (apply string-append
           (table-map t 
                      (λ(n str len) 
                        (string-pad-right (->string (list-ref row n)) len))
                      (table-sep t) (table-sep t) (table-sep t)
                      ))))

(define (print-table-head t)
  (displayln
   (apply string-append
          (table-map t 
                     (λ(n str len)
                       (string-pad-right (->string str) len))
                     (table-sep t) (table-sep t) (table-sep t))
          )))

(define (print-table-line t getter)
  (displayln
    (apply string-append
           (apply table-map t 
                  (λ(n str len) 
                    (build-string len (λ(n)(table-dash t))))
                  (getter t)
                  ))))
(define (print-table-first-line t)
  (print-table-line t table-first-line))
(define (print-table-mid-line t)
  (print-table-line t table-mid-line))
(define (print-table-last-line t)
  (print-table-line t table-last-line))

;; borders: (or 'normal 'rounded 'double)
(define (table-framed head [borders 'rounded])
  (apply table head
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
  (print-table-first-line t1)
  (print-table-head t1)
  (print-table-mid-line t1)
  (print-table-mid-line t1)
  (print-table-row t1 '(a b c))
  (print-table-mid-line t1)
  (print-table-row t1 '(x y "z"))
  (print-table-last-line t1)
  )
