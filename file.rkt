#lang racket/base
(require racket/file
         racket/list
         racket/string
         racket/path
         bazaar/debug)

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

;; Retuns the first lines of the file f as a list of strings
(define (file-head f [n-lines 10])
  (with-input-from-file f
    (λ()(for/list ([line (in-lines)]
                   [i (in-range n-lines)])
          line))))

;; read-data-thunk takes no argument and must return a single value that must be
;; writable to and readable from a file.
;; This value is stored in cache-file for faster loading next time.
;; If cache-file is younger that data-file, it loads the data from cache-file
;; directly and read-data-thunk is not called.
;; If cache-file does not exist or is older than data-file, read-data-thunk
;; is called as data-file is opened for input, and the result of the thunk
;; is both returned and written into cache-file.
(define (with-input-from-file/cache data-file
          read-data-thunk
          #:cache-path [cache-file (path-add-extension data-file #".cache")]
          #:verbose? [verbose? #t])
  (assert (file-exists? data-file) data-file)
  (assert (procedure-arity-includes? read-data-thunk 0))
  (cond
    [(or (not (file-exists? cache-file))
         (< (file-or-directory-modify-seconds cache-file)
            (file-or-directory-modify-seconds data-file)))
     (when verbose?
       (displayln "Generating cache file: Cache file does not exist or is obsolete.")
       (debug-vars data-file cache-file))
     (define D
       (with-input-from-file data-file
         read-data-thunk))
     (write-to-file D cache-file #:exists 'replace)
     D]
    [else (file->value cache-file)]))
