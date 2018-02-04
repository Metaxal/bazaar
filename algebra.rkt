#lang racket

;; This is just an experiment.
;; For an actual Computer Algebra System in Scheme, see:
;; https://github.com/dharmatech/mpl/blob/master/test.sls 

;; +inf.0 is considered a float, and thus (* 0 +inf.0) produces 0 in Racket.
;; This may not be good enough for algebra.
;; An option is to add +inf and -inf as limit integers.

(provide op-sym->op evaluate reduce diff substitute beautify ->symbol)

;; Useful to make symbols
(define (->symbol . l)
    (string->symbol (apply string-append (map ~a l))))

(define (op-sym->op sym)
  (case sym
    [(+) +] [(-) -] [(*) *] [(/) /]
    [(exp) exp] [(log) log] [(expt) expt] [(sqrt) sqrt] [(sqr) sqr]
    [else #f]))

;; Tries to calculate the value of the expression given a dictionary of values for the
;; free symbols.
;; Produces a real value. To obtain an algebraic value, use
;; (reduce (substitute alg-expr dic))
(define (evaluate alg-expr dic)
  (let loop ([x alg-expr])
    (cond [(list? x) (apply (loop (first x))
                            (map loop (rest x)))]
          [(op-sym->op x) => identity]
          [(symbol? x) (dict-ref dic x)]
          [else x])))

(define (substitute expr var.val-dict)
  (let loop ([expr expr])
    (cond [(list? expr)
           (map loop expr)]
          [(number? expr)
           expr]
          [(dict-ref var.val-dict expr #f) => values]
          [else expr])))

;; Receives a bin-expr. Use `reduce' first if not in binary tree format.
;; TODO: LIST ASSUMPTIONS
(define (diff expr [sym 'x])
  (let diff ([expr expr])
    (match expr
      [(? number?)
       0]
      [(? symbol?)
       (if (eq? expr sym) 1 0)]
      [(list (and op (or '+ '-)) a b)
       (list op (diff a) (diff b))]
      [(list '* f g)
       `(+ (* ,(diff f) ,g)
           (* ,f ,(diff g)))]
      #;[(list '/ f g)
       `(- (/ ,(diff f) ,g)
           (/ (* ,f ,(diff g))
              (expt ,g 2)))]
      [(list 'exp f)
       `(* ,(diff f) (exp ,f))]
      [(list 'expt x (? number? a))
       ; Not necessary, but faster
       `(* ,a (* ,(diff x) (expt ,x (- ,a 1))))]
      [(list 'expt f g)
       `(+ (* (* ,g (expt ,f (- ,g 1)))
              ,(diff f))
           (* (expt ,f ,g)
              (* (log ,f)
                 ,(diff g))))]
      [(list 'log f)
       (list '* (diff f) (list 'expt f -1))])))

(define-syntax-rule (match/debug expr [test body ...] ...)
  (let ([expr-val expr])
    (newline)
    (displayln expr-val)
    (match expr-val
      [test
       (displayln 'test)
       body ...]
      ...)))

;; TODO: 3 modes about assumptions:
;; 1) Attempt most common reductions while satisfying the input assumptions,
;;    and list additional assumptions to be satisfied for the reductions to hold.
;; 2) When uncertain, ask the user if the assumption for the current reduction is
;;    fulfilled. If yes, perform reduction and add the assumption. If no, don't reduce.
;; 3) Don't reduce when uncertain.
(define (reduce expr [assumptions '()])
  (define (add-assumptions! . l)
    (set! assumptions
          (append assumptions l)))
  (define new-expr
    (let top-loop ([expr expr])
      ; Process from the leaves up to the root
      (let loop ([expr (if (list? expr)
                           (map top-loop expr)
                           expr)])
        (match #;match/debug expr
          [(? (negate list?) _)
           expr]
          [(list-rest (and op (or '+ '*)) a b c lexpr)
           ; Replace n-ary-operators with binary-operators
           (loop (list op a (loop (list* op b c lexpr))))]
      
          [(list-rest '- a b lexpr)
           ; Don't keep negatives
           (loop (list '+ a
                       (loop (list '* -1 (loop (list* '+ b lexpr))))))]
          [(list '- a)
           (loop (list '* -1 a))]
          [(list-rest '/ a b lexpr)
           ; Don't keep divisions
           (define c (loop (list* '* b lexpr)))
           (add-assumptions! `(!= 0 ,c))
           (loop (list '* a (loop (list 'expt c -1))))]
          [(list '/ a)
           (loop (list 'expt a -1))]

          ; Renamings
          [(list '^ a b)
           (loop 'expt (a b))]
          [`(sqr ,x)
           (loop `(expt ,x 2))]
          [`(sqrt ,x)
           (loop `(expt ,x 1/2))]
          [`(exp ,x)
           (loop (list 'expt 'euler.0 x))]

          ;; Numeric reduction
          ;; WARNING: We should not reduce irrational expressions when given exact numbers!
          ;; (if floats are involved, ok.)
          [(list (? op-sym->op op) (? number? a))
           (loop ((op-sym->op op) a))]
          [(list (? op-sym->op op) (? number? a) (? number? b))
           (loop ((op-sym->op op) a b))]
          [(list (and op (or '+ '*)) x (? number? a))
           ; always place numbers first
           (loop (list op a x))]
          [(list '* 0 x)
           ; Racket always consider that 0 = (* 0 -inf.0) and (* 0. -inf.0) = +nan.0
           #;(add-assumptions! `(!= ,x -inf.0)
                               `(!= ,x +inf.0))
           0]
          [(list '* 0. x)
           ; Racket always consider that 0 = (* 0 -inf.0) and (* 0. -inf.0) = +nan.0
           (add-assumptions! `(!= ,x -inf.0)
                             `(!= ,x +inf.0))
           0.]
          [`(exp (log ,x))
           (add-assumptions! `(> ,x 0))
           (loop x)]
          [(or (list '+ x)
               (list '+ (or 0 0.) x)
               (list '* x)
               (list '* (or 1 1.) x)
               (list 'expt x (or 1 1.)))
           (loop x)]
          [(or `(log euler.0))
           1]
          [`(expt ,x ,(or 0 0.)) ; includes 0^0
           1]
          [(list 'expt 1 x)
           1]
          [`(expt 0 ,a)
           (add-assumptions! `(!= -inf.0 ,a) `(!= 0 ,a))
           0]
          [`(log (expt ,a ,b))
           (loop (list '* b (loop (list 'log a))))]
      
          [(list '+ x x)
           (loop `(* 2 ,x))]
          [`(+ ,x (+ ,x ,y))
           (loop (list '+ y (loop (list '* 2 x))))]
          [(list '* x x)
           (loop `(expt ,x 2))]
          [`(* ,x (* ,x ,y))
           (loop (list '* y (loop `(expt ,x 2))))]
          [(or `(* ,x (expt ,x ,a))
               `(* (expt ,x ,a) ,x))
           (loop (list 'expt x (loop (list '+ a 1))))]
          [`(* (expt ,x ,a) (expt ,x ,b))
           (loop `(expt ,x ,(loop `(+ ,a , b))))]
          [`(/ (expt ,x ,a) (expt ,x ,b))
           (loop `(expt ,x ,(loop `(- ,a ,b))))]
          [`(* (expt ,a ,x) (expt ,b ,x))
           (loop (list 'expt (loop (list '* a b)) x))]
          [`(expt (expt ,a ,b) ,c)
           (loop (list 'expt a (loop (list '* b c))))]
          [`(+ (* ,a ,b) (* ,a ,c))
           ; Factorize
           (loop (list '* a (loop (list '+ b c))))]
          [`(+ (* ,a ,b) (* ,c ,b))
           ; Factorize
           (loop (list '* (loop (list '+ a c)) b))]
          [(or `(+ ,x (* ,a ,x))
               `(+ (* ,a ,x) ,x))
           (loop (list '* (loop (list '+ a 1)) x))]
      
          [(list (and op (or '+ '*))
                 (? number? a)
                 (list op (? number? b) x))
           (loop (list op ((op-sym->op op) a b) x))]
          [(list (and op (or '+ '*))
                 (list op a b)
                 c)
           ; linearize
           (loop (list op a (loop (list op b c))))]
          [(list (and op (or '+ '*))
                 (? (negate number?) x)
                 (list op (? number? a) y))
           (loop (list op a (loop (list op x y))))]
          [(list (and op (or '+ '*)) (? symbol? x) (? symbol? y))
           ; Reorder alphabetically
           (if (symbol<? y x)
               (loop (list op y x))
               expr)]
          [(list (and op (or '+ '*))
                 (? symbol? x)
                 (list op
                       (? symbol? y)
                       z))
           (if (symbol<? y x)
               (list op y (loop (list op x z)))
               expr)]
          [else expr]))))
  (unless (empty? assumptions)
    (displayln 'Assumptions)
    (for-each displayln assumptions))
  new-expr)

;; todo: Beautify, to undo some canonization done by reduce,
;; like to replace (* -1 f) with (- f) and (expt f -1) with (/ f),
;; (* a (* b c)) with (* a b c), etc.
;; (expt euler.0 f) with (exp f).
(define (beautify expr)
  ; Process from the leaves up to the root
  (let loop ([expr (if (list? expr)
                       (map beautify expr)
                       expr)])
    (#;match/debug match expr
      [`(expt euler.0 ,a)
       (loop `(exp ,a))]
      [`(+ ,a (+ ,b ,c))
       (loop `(- ,a ,b ,c))]
      [`(* ,a (* ,b ,c))
       (loop `(* ,a ,b ,c))]
      [`(* ,(or -1 -1.) ,a)
       (loop `(- ,a))]
      [`(expt ,a ,(or -1. -1))
       (loop `(/ ,a))]
      [`(expt ,a ,(or 2 2.))
       (loop `(sqr ,a))]
      [`(expt ,a ,(or 1/2 .5))
       (loop `(sqrt ,a))]
      [`(+ ,(? (λ(x)(and (number? x)
                         (negative? x))) n)
           ,a)
       (loop `(- ,a ,(- n)))]
      [else expr])))


(module+ drracket
  (reduce (diff '(expt (+ (expt x 2) 3) 2)))
  (reduce '(* y (/ 3 4 x 5 x y)))

  (beautify (reduce '(* y (/ 3 4 x 5 x y))))
  (define exp1
    (cons '*
          (for/list ([i (in-range 4)])
            (define wi (string->symbol (format "w~a" i)))
            (define ni (string->symbol (format "n~a" i)))
            `(expt ,wi (- ,ni))))))
