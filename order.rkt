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
(module+ test (require rackunit))

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

(define (atom? a)
  (or (number? a) (string? a) (symbol? a) (char? a) (null? a)))

(define (atom<=> a b)
  (cond [(and (number? a) (number? b)) (number<=> a b)]
        [(and (string? a) (string? b)) (string<=> a b)]
        [(and (symbol? a) (symbol? b)) (symbol<=> a b)]
        [(and   (char? a)   (char? b))   (char<=> a b)]
        [(and   (null? a)   (null? b)) '=]
        [else #f]))

;; Takes time linear with the smallest list.
(define (length<=> l1 l2)
  (cond [(and (not (pair? l1)) (not (pair? l2))) '=]
        [(not (pair? l1)) '<]
        [(not (pair? l2)) '>]
        [else (length<=> (cdr l1) (cdr l2))]))

;; Returns the opposite of a comparison value.
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

  (define (check<=> <=> a b res)
    (check-eq? (<=> a b) res)
    (check-eq? (<=> b a) (<=>opposite res)))
  
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

