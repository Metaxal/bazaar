#lang racket/base
;;; Copyright (C) Laurent Orseau, 2010-2013
;;; GNU Lesser General Public Licence (http://www.gnu.org/licenses/lgpl.html)

(require racket/file
         racket/list
         racket/string
         racket/path)

(provide (all-defined-out))

;; Find all files in base-dir (see find-files)
;; that match at least one pattern in 'patterns'
;; and does not match any of the patterns in ignore-patterns.
(define (find-files-ex patterns [ignore-patterns '()]
                       [base-dir #f])
  (find-files 
   (λ(f)(and (ormap (λ(pat)(regexp-match pat f)) patterns)
             (not (ormap (λ(pat)(regexp-match pat f)) ignore-patterns))))
   base-dir))

#| Example:
; Search for all 'png' files in a directory 'img', but not in '.svn' directories:
 (find-files-ex
   '(#rx"img/.*\\.png")
   '(#rx"\\.svn")
   )
|#


;; list list -> prefix l1-rest l2-rest
;; prefix: a list of the first equal elements of l1 and l2
;; l1-rest and l2-rest are the remaining elements after prefix
(define (most-common-prefix l1 l2)
  (let loop ([prefix '()]
             [l1 l1]
             [l2 l2])
    (cond [(or (empty? l1) (empty? l2)
               (not (equal? (first l1) (first l2))))
           (values prefix l1 l2)]
          [else (loop (append prefix (list (first l1)))
                      (rest l1)
                      (rest l2))])))

;; base-dir: string-or-path, base directory.
;; a-path: string-or-path, file or directory
;; -> path?, a path that represents a-path, relatively to base-dir
(define (relative-path base-dir path)
  (let ([lbase (explode-path (normal-case-path (simple-form-path base-dir)))]
        [lpath (explode-path (normal-case-path (simple-form-path path)))])
    (let-values ([(common rest-base rest-path)
                  (most-common-prefix lbase lpath)])
      (apply build-path (append (map (λ _ 'up) rest-base) rest-path))
    )))

#| Tests: | #
(relative-path
 "/a/b/c/d/f/g"
 "/a/b/c/d/e/g/h"
 )
;|#

;; Writes a path constructor from a path
(define (write-path p)
  (cons 'build-path 
        (map (λ(p-elt)(cond [(symbol? p-elt) (list 'quote p-elt)]
                            [(absolute-path? p-elt) (path->string p-elt)]
                            [else (path-element->string p-elt)]))
             (explode-path p))))

#|
> (build-path 'same 'up "a" "b")
#<path:./../a/b>
> (write-path (build-path 'same 'up "a" "b"))
'(build-path 'same 'up "a" "b")
|#

;; Replaces the content of a file by the same content where each line is parsed with regexp-replace
(define (file-replace f from to #:all? [all? #t])
  (display-to-file (string-replace (file->string f) from to #:all? all?)
                   f
                   #:exists 'replace))

#|
(require racket/port)
(let ([f (make-temporary-file)])
  (display-to-file "Let's call a cat a cat" f #:exists 'replace)
  (file-replace f "cat" "dog")
  (display-lines (file->lines f))
  (file-replace f " a" " the" #:all? #f)
  (display-lines (file->lines f))
  )
;|#
