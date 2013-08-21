#lang racket/base

; Taken from /collects/scribble/tools/drracket-buttons.rkt

(require racket/runtime-path
         ;racket/class
         racket/path
         racket/system
         setup/xref
         ;net/sendurl
         )

(provide scribble->PDF
         scribble->HTML)

(define-namespace-anchor anchor)

; file should be a full-path
(define (scribble-compile file mode suffix extra-cmdline)
  ;(printf "A\n")
  (let-values ([(out) (current-output-port)]
               [(p) (open-output-string)]
               [(base name dir?) (split-path file)])
    (parameterize ([current-namespace (make-base-namespace)]
                   [current-output-port p]
                   [current-error-port p]
                   [current-directory 
                    (path-only (path->complete-path file))]
                   [current-command-line-arguments
                    (list->vector 
                     (append
                      extra-cmdline
                      ;(list "--quiet")
                      (list mode 
                            (path->string (file-name-from-path file)))))])
      ;(fprintf out "cla:~a\n" (current-command-line-arguments))
      ;(fprintf out "currdir:~a\n" (current-directory))
      (namespace-attach-module (namespace-anchor->empty-namespace anchor) 'setup/xref)
      ;(fprintf out "C\n")
      (dynamic-require 'scribble/run #f)
      )
    ;    (current-output-port out)
    ;(fprintf out "D\n")
    ; return:
    (displayln (get-output-string p))
    ))

(define (scribble->HTML file)
  (scribble-compile file "--html" #".html" 
                    '("++xref-in" "setup/xref" "load-collections-xref")))

(define (scribble->PDF file)
  (scribble-compile file "--pdf" #".pdf" null))

#;(cond
  [(equal? suffix #".html")
   (send-url/file (path-replace-suffix fn suffix))]
  [else
   (system (format "open ~s" (path->string (path-replace-suffix fn suffix))))])

#;(let ([html-button
         (make-render-button "Scribble HTML" html.png "--html" #".html" 
                             '("++xref-in" "setup/xref" "load-collections-xref"))]
        [pdf-button
         ;; only available on OSX currently
         ;; when we have a general way of opening pdfs, can use that
         (make-render-button "Scribble PDF" pdf.png "--pdf" #".pdf" null)]))
