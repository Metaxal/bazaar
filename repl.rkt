#lang racket/base

(require define2
         racket/contract/base
         racket/port)

(provide/contract
 [eval-print (-> list? namespace? any/c)]
 [repl (->* ()
            (namespace? #:commands list? #:prompt string?)
            any/c)])

;; TODO: Extract to its own package

;; Evaluates each of the expressions of xs in the namespace ns
;; and prints the results (unless there is only one result and it is void?).
;; xs is a list of s-expressions
;; ns is a namespace
(define (eval-print xs ns)
  (for ([x (in-list xs)])
    (define res (call-with-values (λ () (eval x ns)) list))
    (unless (and (= 1 (length res)) (void? (car res)))
      (for-each writeln res))))

;; ns is a namespace.
;; commands is a list of commands to initialize the repl.
(define (repl [ns (current-namespace)]
              #:commands [commands '()]
              #:prompt [prompt "> "])
  (eval-print commands ns)
  (let loop ()
    (display prompt)
    (with-handlers ([exn:fail? (λ (e) (displayln (exn-message e))
                                 (loop))])
      (define str (read-line))
      (unless (or (eof-object? str)
                  (string=? str ",exit"))
        (define xs
          (with-input-from-string (string-append "(" str ")")
            read))
        (eval-print xs ns)
        (loop)))))

;; Kind of 'the right way', but it's less flexible and fails in DrRacket
#;(define (namespace-anchor->repl a dic)
    (define ns (namespace-anchor->namespace a))
    (λ ()
      (parameterize ([current-namespace ns])
        (read-eval-print-loop))))

(module+ test
  (require rackunit)
  (define-namespace-anchor a)
  (define ns (namespace-anchor->namespace a))
  (check-equal? (with-output-to-string (λ () (eval-print '((+ 3 4)
                                                           (displayln "youpi")
                                                           (values 3 4))
                                                         ns)))
                "7\nyoupi\n3\n4\n"))
