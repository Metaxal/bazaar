#lang racket/base
;;; Copyright (C) Laurent Orseau, 2010-2013
;;; GNU Lesser General Public Licence (http://www.gnu.org/licenses/lgpl.html)

(require "loop.rkt"
         (only-in srfi/13 string-pad-right)
         racket/string
         racket/list
         racket/format
         racket/dict)

(provide (all-defined-out))

;; Simple table to string conversion, ensuring consistent column width
;; See below for more complex ones.
(define (table->string ll
                       #:->string [->string ~a]
                       #:col-sep [col-sep " "] ; string?
                       #:row-sep [row-sep #f] ; #f or char?
                       #:align [align 'left]) ; like for ~a
  (let* ([lens (map length ll)]
         [len1 (first lens)])
    (unless (andmap (λ(len)(= len len1)) (rest lens))
      (error "All rows must have the same length")))
  (define/for/fold ([cell-sizes (make-list (length (first ll)) 0)]
                    [ll-str '()])
                   ([row (in-list ll)])
    (define/for/fold ([new-cell-sizes '()]
                      [row-str '()])
                     ([cell (in-list row)]
                      [size (in-list cell-sizes)])
      (define str (->string cell))
      (values (cons (max (string-length str) size) new-cell-sizes)
              (cons str row-str)))
    (values (reverse new-cell-sizes)
            (cons (reverse row-str) ll-str)))
  (define rows-str
    (map (λ(row-str)
           (string-join (map (λ(str size)
                               (~a str
                                   #:min-width size
                                   #:align align))
                             row-str
                             cell-sizes)
                        col-sep))
         (reverse ll-str)))
  (string-join
   (if row-sep
       (add-between rows-str (make-string (string-length (first rows-str)) row-sep))
       rows-str)
   "\n"))


(module+ main
 (displayln
  (table->string
   '((a b c d e f g h)
     (123 456 77 54 1  5646547987 41 1)
     (111 22 3333 44 5 6 7 8888))
   #:align 'right
   #:col-sep " │ "
   #:row-sep #\—)))


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
                      (string-pad-right (~a (list-ref row n)) len))
                    (atable-sep t) (atable-sep t) (atable-sep t))))

(define (table-head t)
  (apply string-append
         (table-map t 
                    (λ(n str len)
                      (string-pad-right (~a str) len))
                    (atable-sep t) (atable-sep t) (atable-sep t))))

(define (table-line t getter)
  (apply string-append
         (apply table-map t 
                (λ(n str len) 
                  (build-string len (λ(n)(atable-dash t))))
                (getter t))))
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
                  (table-last-line t1))))
