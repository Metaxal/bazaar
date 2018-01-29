#lang racket

(define (op-sym->op sym)
  (case sym
    [(+) +] [(-) -] [(*) *] [(/) /] [(exp) exp] [(log) log] [(expt) expt]
    [else #f]))

;; Receives a bin-expr. Use `reduce' first if not in binary tree format. 
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

(define (reduce expr)
  ; Process from the leaves up to the root
  (let loop ([expr (if (list? expr)
                       (map reduce expr)
                       expr)])
    (#;match/debug match expr
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
       (loop (list '* a (loop (list 'expt (loop (list* '* b lexpr)) -1))))]
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
      
      [(list (? op-sym->op op) (? number? a))
       (loop ((op-sym->op op) a))]
      [(list (? op-sym->op op) (? number? a) (? number? b))
       (loop ((op-sym->op op) a b))]
      [(list (and op (or '+ '*)) x (? number? a))
       ; always place numbers first
       (loop (list op a x))]
      [(list '* 0 x) ; x != -inf.0, x != +inf.0
       0]
      [(or (list '+ x)
           (list '+ 0 x)
           (list '* x)
           (list '* 1 x)
           `(exp (log ,x)) ; x > 0
           (list 'expt x 1))
       (loop x)]
      [(or `(log euler.0))
       1]
      [(or (list 'expt 1 x)
           (list 'expt x 0)) ; x != -inf.0 ?
       1]
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
      [else expr])))

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
      [`(* -1 ,a)
       (loop `(- ,a))]
      [`(expt ,a -1)
       (loop `(/ ,a))]
      [`(+ ,(? (λ(x)(and (number? x)
                         (negative? x))) n)
           ,a)
       (loop `(- ,a ,(- n)))]
      [else expr])))

#;(reduce (diff '(expt (+ (expt x 2) 3) 2)))
#;(reduce '(* y (/ 3 4 x 5 x y)))

