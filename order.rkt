#lang racket/base
(require racket/math
         racket/list
         (submod racket/performance-hint begin-encourage-inline))

(provide (all-defined-out))

;***************************************************************************************;
;****                        Order Relations And Comparators                        ****;
;***************************************************************************************;
;;; Also see data/order

;;; TODO: docs
(module+ test
  (require rackunit)
  (provide check<=>)
  
  (define-check (check<=> <=> a b res)
    (define got1 (<=> a b))
    (unless (eq? got1 res)
      (fail-check (format "<=> a b: expected ~a got ~a" res got1)))
    (define res2 (<=>opposite res))
    (define got2 (<=> b a))
    (unless (eq? got2 res2)
      (fail-check (format "<=> b a: expected ~a got ~a" res2 got2)))))

;;; A comparator is a binary procedure that returns one of '(< > = #f).
;;; The value #f is for when the inputs cannot be compared.
;;; A comparator's name typically ends with <=>.
(begin-encourage-inline
  (define (order<? c) (eq? c '<))
  (define (order>? c) (eq? c '>))
  (define (order=? c) (eq? c '=))
  (define (order<=? c) (or (eq? c '<) (eq? c '=)))
  (define (order>=? c) (or (eq? c '>) (eq? c '=)))
  (define (order<>? c) (or (eq? c '<) (eq? c '>))) ; neither '= nor #f
  (define order≤? order<=?)
  (define order≥? order>=?))

;; Returns a comparator based on <?.
;; If fkey is a procedure of one argument, then the comparator returns #f
;; whenever either input argument is not #f according to fkey.
;; fkey: (or/c #f (procedure-arity-includes/c 1))
(define (make<=> <? #:key-false [fkey #f])
  (λ (a b)
    (cond
      [(and fkey (or (fkey a) (fkey b))) #f]
      [(<? a b) '<]
      [(<? b a) '>]
      [else '=])))

(define number<=> (make<=> < #:key-false nan?))
(define string<=> (make<=> string<?))
(define symbol<=> (make<=> symbol<?))
(define   char<=> (make<=> char<?))

(define (boolean<=> a b)
  #;'((#f #f =)
      (#f #t <)
      (#t #f >)
      (#t #t =))
  (if (eq? (and a #t) (and b #t))
    '=
    (if b '< '>)))

(module+ test
  (check<=> boolean<=> 4 #t '=)
  (check<=> boolean<=> 4 #f '>)
  (check<=> boolean<=> #f #t '<)
  (check<=> boolean<=> #t #t '=)
  (check<=> boolean<=> #f #f '=))

(define (atom? a)
  (or (boolean? a) (number? a) (string? a) (symbol? a) (char? a) (null? a)))

(define (atom<=> a b)
  (cond [(and (boolean? a) (boolean? b)) (boolean<=> a b)]
        [(and  (number? a)  (number? b))  (number<=> a b)]
        [(and    (char? a)    (char? b))    (char<=> a b)]
        [(and  (symbol? a)  (symbol? b))  (symbol<=> a b)]
        [(and  (string? a)  (string? b))  (string<=> a b)]
        [(and    (null? a)    (null? b))               '=]
        [else #f]))

;; Takes time linear with the smallest list.
(define (length<=> l1 l2)
  (cond [(and (not (pair? l1)) (not (pair? l2))) '=]
        [(not (pair? l1)) '<]
        [(not (pair? l2)) '>]
        [else (length<=> (cdr l1) (cdr l2))]))

;; Returns the opposite of a comparison value, that is, for some comparator <=>,
;; (<=>opposite (<=> a b)) == (<=> b a)
(define (<=>opposite c)
  (case c
    [(<) '>]
    [(>) '<]
    [else c])) ; '= or #f

(module+ test
  (check-true (order<? (number<=> 0 1)))
  (check-true (order≤? (number<=> 0 1)))
  (check-true (order=? (number<=> 5 5)))
  (check-true (order>? (number<=> 1 0)))
  (check-true (order≥? (number<=> 1 0)))
  (check-false (order≥? (number<=> 0 1)))
  
  (check<=> string<=> "abc" "abd" '<)
  (check<=> length<=> 'a 'b '=)
  (check<=> length<=> '() '(a) '<)
  (check<=> length<=> '() '() '=)
  (check<=> length<=> '(a) '(a) '=)
  (check<=> length<=> '((a b)) '(a) '=)
  (check<=> length<=> '(a a a) '(a) '>))


;; Given a sequence of comparators, returns a new comparator that
;; applies the first comparator and returns its value unless it is '=,
;; in which case it applies the second comparator and so on.
;; If the last comparator is applied and returns '=, then it is the return value.
;; The minimum number of comparisons is performed.
(define-syntax chain-comparisons
  (syntax-rules ()
    [(_ cmp) cmp]
    [(_ cmp1 cmp2 ...)
     (let ([v cmp1])
       (case v
         [(#f) #f] ; incomparable
         [(=) (chain-comparisons cmp2 ...)]
         [else v]))]))

(module+ test
  (check-equal?
   (let ([l1 '(a 2 #\C)])
     (for/list ([l2 (in-list (list '(b 1 #\C)
                                   '(a 1 #\D)
                                   '(a 2 #\D)
                                   '(a 2 #\C)))])
       (chain-comparisons (symbol<=> (first l1)  (first l2))
                          (number<=> (second l1) (second l2))
                          (char<=>   (third l1)  (third l2)))))
   '(< > < =)))

;; Going through l1 and l2 in parallel, return the first comparison
;; that is not '=.
;; If l1 is shorter (longer) than l2, then return '< ('>).
;; The name is by similarity with `andmap` and `ormap`.
(define (<=>map <=> l1 l2)
  (let loop ([l1 l1] [l2 l2])
    (define e1 (empty? l1))
    (define e2 (empty? l2))
    (cond [(and e1 e2) '=]
          [e1 '<]
          [e2 '>]
          [else
           (define c (<=> (car l1) (car l2)))
           (if (order=? c)
             (loop (cdr l1) (cdr l2))
             c)])))

(module+ test
  (define (<=>mapper <=>)
    (λ (l1 l2) (<=>map <=> l1 l2)))
  (check<=> (<=>mapper number<=>) '(1 2 3 4 5) '(1 2 4) '<)
  (check<=> (<=>mapper number<=>) '(1 2 3 4 5) '(1 2 3) '>)
  (check<=> (<=>mapper number<=>) '(1 2 3 4 5) '(1 2 3 4 5) '=)
  (check<=> (<=>mapper number<=>) '() '() '=))

  