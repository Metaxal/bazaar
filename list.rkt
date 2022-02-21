#lang racket/base

(require define2
         racket/list
         racket/contract
         racket/generator
         racket/sequence
         syntax/parse/define)

(provide (all-defined-out))

(module+ test
  (require "rackunit.rkt"
           racket/set
           math/number-theory))

;; Take Θ(min(length(l1), length(l2)) instead of Θ(length(l1) + length(l2))
(define (length<? l1 l2)
  (cond [(null? l2) #false]
        [(null? l1) #true]
        [else (length<? (cdr l1) (cdr l2))]))

(module+ test
  (check-true (length<? '() '(a)))
  (check-true (length<? '(a) '(a b)))
  (check-false (length<? '() '()))
  (check-false (length<? '(a) '(a)))
  (check-false (length<? '(a b) '(a))))


;; OBSOLETE: Use random-ref and random-sample from racket/random instead
(define (choose l)
  (if (empty? l)
      (error "List must not be empty")
      (list-ref l (random (length l)))))

;; Fast version of append that reverses the order of the elements of l1 into l2.
;; Useful when l1 is known to be reversed already.
;; It is most efficient when l1 is shorter than l2.
(define (rev-append l1 l2)
  (if (null? l1)
      l2
      (rev-append (cdr l1) (cons (car l1) l2))))

(module+ test
  (check-equal? (rev-append '() '())
                '())
  (check-equal? (rev-append '(a b c) '(1 2 3))
                '(c b a 1 2 3))
  (check-equal? (rev-append '() '(1 2 3))
                '(1 2 3))
  (check-equal? (rev-append '(a b c) '())
                '(c b a))
  (check-equal? (rev-append '() '(a b c))
                '(a b c)))

(define (fmax l [f values])
  (for/fold ([fxmax -inf.0])
            ([x (in-list l)])
    (max (f x) fxmax)))

;; Returns the index and value of the >?-maximal element of l.
;; l must be a non-empty list.
;; Use >= instead of > to retrieve the last index
(define (index-max l [>? >])
  (for/fold ([imax 0]
             [vmax (first l)])
            ([i (in-naturals 1)]
             [v (in-list (rest l))])
    (if (>? v vmax)
        (values i v)
        (values imax vmax))))

(module+ test
  (let-values ([(i v) (index-max '(0 4 2 1 8 4))])
    (check-equal? i 4)
    (check-equal? v 8)))

(define (transpose ll)
  (if (empty? ll)
      ll
      (apply map list ll)))

(module+ test
  (check-equal? (transpose '()) '())
  (check-equal? (transpose '((a b c) (1 2 3) (d e f)))
                '((a 1 d) (b 2 e) (c 3 f))))

(define (consr x l)
  (append l (list x)))

;; Like add-between, but after each element
(define (add-after l elt)
  (append-map (λ(e)(list e elt)) l))

(define (add-before l elt)
  (append-map (λ(e)(list elt e)) l))

(define (list->cumul-list l)
  (define sum 0)
  (for/list ([x (in-list l)])
    (set! sum (+ sum x))
    sum))

(module+ test
  (check-equal?
   (list->cumul-list '(2 8 3 5 5 0 -1 2))
   '(2 10 13 18 23 23 22 24)))

;; l: (listof real?)
;; -> (listof real?)
;; Assumes the sum is non-zero
(define (normalize l)
  (define s (apply + l))
  (map (λ(x)(/ x s)) l))

;; Maps a list of lists of elements (keeps the list of list structure).
;; See also tree-map in tree.rkt
(define (map-map proc ll)
  (for/list ([l (in-list ll)])
    (for/list ([x (in-list l)])
      (proc x))))

(module+ test
  (check-equal?
   (map-map add1 '((0 1) (2 3) (5 4)))
   '((1 2) (3 4) (6 5))))

;;; These should be in values.rkt instead

;; Return the multiple values of proc-call as a list
;; See also values->list in "values.rkt"
(define-syntax-rule (call/values->list expr)
  (call-with-values (λ()expr) (λ l l)))
; Example:
; (call/values->list (values 1 2 3))
; -> '(1 2 3)

(define-simple-macro (define-list (var:id ...) e:expr)
  (define-values (var ...) 
    (apply values e)))
; Example:
; (define-list (a b c) (list 1 2 3))

(define-simple-macro (let-list ([var:id ... e:expr] ...) body ...)
  (let-values ([(var ...) (apply values e)] ...) body ...))
; Example:
; (let-list ([x y z '(a b c)])
;   (list z y x))
;-> '(c b a)

;; Returns the first element e in l such that (=? x (key e)), or not-found otherwise.
(define (find x l #:key [key values] #:=? [=? equal?] #:not-found [not-found #f])
  (let loop ([l l])
    (if (empty? l)
        not-found
        (let ([e (first l)])
          (if (=? x (key e))
              e
              (loop (rest l)))))))

(module+ test
  (let ([l '((a 1)(b 2)(c 3)(d 2))])
    (check-equal? (find 'b l #:key first)
                  '(b 2))
    (check-equal? (find '(b 2) l)
                  '(b 2))
    (check-equal? (find '(b 3) l #:not-found 'none)
                  'none)
    ))

(define (replace l a b
                   #:=? [=? equal?])
    (map (λ(x)(if (=? x a) b x)) l))

(module+ test
  (check-equal? (replace '(a b c b c d) 'b 3)
                '(a 3 c 3 c d))
  (check-equal? (replace '(a b c b c d) 'e 3)
                '(a b c b c d))
  (check-equal? (replace '() 'e 3)
                '()))

;; Replaces the last element of l by x.
;; If l is empty, the empty list is returned.
(define/contract (replace-last l x)
  (list? any/c . -> . list?)
  (cond [(null? l) '()]
        [(null? (cdr l)) (list x)]
        [else (cons (car l) (replace-last (cdr l) x))]))

(module+ test
  (check-equal? (replace-last '() 'a) '())
  (check-equal? (replace-last '(a b c) 'a) '(a b a)))

(define/contract (remove-last l)
  ((and/c list? (not/c empty?)) . -> . list?)
  (cond [(null? l) '()]
        [(null? (cdr l)) '()]
        [else (cons (car l) (remove-last (cdr l)))]))

(module+ test
  (check-fail (remove-last '()))
  (check-equal? (remove-last '(a b c)) '(a b)))


;; l : list of numbers.
;; αt : (or procedure-arity-1 number-in-[0,1]) : weight of the past, = 1 - weight of current number.
;;   First element has t-index 0. By default the rolling average is a uniform average of all numbers
;;   up to the current one.
(define (rolling-average l [αt (λ(t)(/ t (+ t 1)))])
  (let ([αt (if (number? αt) (λ(t)αt) αt)])
    (if (empty? l)
        '()
        (let loop ([l (rest l)]
                   [t 1]
                   [l2 (list (first l))]
                   [avg (first l)])
          (if (empty? l)
              (reverse l2)
              (let* ([α (αt t)]
                     [new-avg (+ (* α avg) (* (- 1 α) (first l)))])
                (loop (rest l)
                      (+ t 1)
                      (cons new-avg l2)
                      new-avg)))))))

(module+ test
  (let ([l (range 10)])
    (check-equal? (rolling-average l)
                  (map (λ(i)(/ i 2)) l))
    (check-equal? (rolling-average l 1/2)
                  (build-list (length l) (λ(i)(+ i -1 (expt 2 (- i))))))))



;; All binary sequences of length T containing exactly k elements.
;; There are (binomial T k) such sequences.
(define (in-binary-lists T k)
  (in-generator
   (let loop ([l '()] [t 0] [n1 0])
     (if (= t T)
         (yield l)
         (begin
           (when (> (+ n1 T (- t))
                    k) ; we will still have room for the ones later if we place a zero right now
             (loop (cons 0 l) (+ t 1) n1))
           (when (< n1 k) ; we can still place some ones
             (loop (cons 1 l) (+ t 1) (+ n1 1))))))))

(module+ test
  (check set=?
         (sequence->list (in-binary-lists 5 2))
         '((1 1 0 0 0)
           (1 0 1 0 0)
           (0 1 1 0 0)
           (1 0 0 1 0)
           (0 1 0 1 0)
           (0 0 1 1 0)
           (1 0 0 0 1)
           (0 1 0 0 1)
           (0 0 1 0 1)
           (0 0 0 1 1))
         )
  (check = (sequence-length (in-binary-lists 10 7))
           (binomial 10 7))
  (check = (sequence-length (in-binary-lists 10 3))
           (binomial 10 3)))


;; Like remove-duplicates but assumes the list is sorted.
;; Turns the search from quadratic to linear (or n log n if we count the cost of sorting).
(define (remove-duplicates-sorted l [=? equal?])
  (if (empty? l)
      '()
      (let loop ([l (rest l)] [res (list (first l))])
        (if (empty? l)
            (reverse res)
            (if (=? (first l) (first res))
                (loop (rest l) res)
                (loop (rest l) (cons (first l) res)))))))
(module+ test
  (let ([l (build-list 100 (λ(i)(random 100)))])
    (check-equal? (sort (remove-duplicates l) <)
                  (remove-duplicates-sorted (sort l <)))))

;; Alias, just a better name
(define remove-adjacent-duplicates remove-duplicates-sorted)

(define (take-at-most l n)
  (for/list ([x (in-list l)]
             [i (in-range n)])
    x))

(module+ test
  (check-equal? (take-at-most '(a b c d) 10)
                '(a b c d))
  (check-equal? (take-at-most '(a b c d) 2)
                '(a b))
  (check-equal? (take-at-most '(a b c d) 0)
                '())
  (check-equal? (take-at-most '() 2)
                '()))

;; A short-hand for the many cases where one has to process a list recursively.
;; See also `zip-loop` in loop.rkt
(define-simple-macro (if-empty-first [l:expr x:id] {empty-body ...} {first-body ...})
  (let ([l2 l]) ; in case l is an expression
    (if (empty? l2)
        (let ()
          empty-body ...)
        (let ([x (first l2)])
          first-body ...))))

(module+ test
  (check-equal?
   (let ([a 0])
     (if-empty-first
      {(let () (set! a (+ a 1)) (list a 2 3)) y}
      {(error "a")}
      {(define x (list 0 y))
       x}))
   '(0 1))
  
  (check-equal?
   (let loop ([l '(3 4 5)])
     (if-empty-first
      [l x]
      {'(a)}
      {(cons (- x) (loop (rest l)))}))
   '(-3 -4 -5 a)))
