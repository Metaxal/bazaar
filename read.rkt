#lang racket/base

(require racket/dict
         racket/list
         racket/vector
         define2)

(provide (all-defined-out))

;; This can read things like #<procedure:string-append>, while the default reader
;; raises an exception.
(define readtable-#<procedure:>
  (make-readtable
   (current-readtable)
   #\<
   'dispatch-macro
   (case-lambda
     [(ch port) ; read-mode only (no read-syntax)
     (define sym (read port))
     (define str (symbol->string sym))
     (vector (string->symbol (substring str 0 (- (string-length str) 1))))]
     [(ch port src line col pos)
      (error "not implemented")])
   ))

(module+ test
  (require rackunit
           racket/port)
  (check-equal?
   (parameterize ([current-readtable readtable-#<procedure:>])
     (with-input-from-string "(i guess #(is this #<procedure:argh> true?) it is)" read))
   '(i guess #(is this #(procedure:argh) true?) it is)))


;; Parses a result of `read` and replaces structs-as-vectors with actual structs,
;; for #transparent structs that were written.
;; (Non-#transparent structs appear as procedure #<struct-constructor> and can be read
;; with the readtable-#<procedure> above, but information is lost since they are opaque).
;; This can't be done automatically by `read` itself because it doesn't know about the
;; struct at read point (the same name could refer to different structs).
;; By passing the actual constructor as argument, the structs can be reconstructed.
;; Notice: vectors (usually immutable when read) are replaced with mutable vectors,
;; and similarly for hashes.
;;
;; `restruct` can also be used to rebind procedures when used in combination with
;; `readtable-#<procedure:> above. See example in the tests below.
;; We could even write function applications.
;;
;; s: A result of `read`.
;; struct-dict: (dict-of symbol? procedure?)
;;  where the key is the symbolic name of the procedure as found in s,
;;  and the value is the constructor to apply to to the elements of s.
(define (restruct s struct-dict)
  (let loop ([s s])
    
    (cond [(and (vector? s)
                (> (vector-length s) 0)
                (dict-ref struct-dict (vector-ref s 0) #f))
           => (λ (constructor) (apply constructor
                                      (map loop (rest (vector->list s)))))]
          [(vector? s) (vector-map loop s)]
          [(pair? s) (cons (loop (car s)) (loop (cdr s)))]
          [(hash? s)
           (define assocs (map (λ (p) (cons (loop (car p)) (loop (cdr p))))
                               (hash->list s)))
           (cond [(hash-equal? s) (make-hash assocs)]
                 [(hash-eqv? s) (make-hasheqv assocs)]
                 [(hash-eq? s) (make-hasheq assocs)])]
          [else s])))

(module+ test
  (require racket/port
           racket/match)
  (let ()
    (struct BB (bb) #:transparent)
    (struct AA (aa bb) #:transparent)
    (define s (list 'x 3 (AA (AA #(1 2 3) (vector (cons 'a (BB 'u))))
                             (hash 'bb (BB 'bb)))))
    (define str (with-output-to-string (λ () (write s))))
    #;(writeln str)
    (define res (with-input-from-string str (λ () (read))))
    (check-true
     (match (restruct res `((struct:AA . ,AA)
                            (struct:BB . ,BB)))
       [(list 'x 3 (AA (AA '#(1 2 3) (vector (cons 'a (BB 'u))))
                       (hash-table ('bb (BB 'bb)))))
        #t])))

  (let ()
    (struct BB (bb) #:transparent)
    (struct AA (aa bb) #:transparent)
    (define (foo x y) (list y x))
    (define s (list 'x 3 (AA (AA #(1 2 3) (BB 'u))
                             (BB 'bb))
                    foo))
    (define str (with-output-to-string (λ () (write s))))
    #;(writeln str)
    (define res
      (parameterize ([current-readtable readtable-#<procedure:>])
        (with-input-from-string str (λ () (read)))))
    (check-true
     (match (restruct res `((struct:AA . ,AA)
                            (struct:BB . ,BB)
                            (procedure:foo . ,(λ () foo))))
       [(list 'x 3 (AA (AA '#(1 2 3) (BB 'u)) (BB 'bb)) the-foo)
        (equal? the-foo foo)])))
  )



