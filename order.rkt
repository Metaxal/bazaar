#lang racket/base

(require define2
         (for-syntax racket/base)
         racket/math
         racket/list
         (submod racket/performance-hint begin-encourage-inline))

(provide (all-defined-out))

;***************************************************************************************;
;****                        Order Relations And Comparators                        ****;
;***************************************************************************************;
;;; See also https://srfi.schemers.org/srfi-67/srfi-67.html#node_idx_90
;;; but only meant for total orders (values are {-1, 0, 1} instead of {'<, '=, '>, #f}).
;;; and data/order (also total order).
;;;
;;; A comparator <=> must ensure that
;;; if (<=> a b) = '< then (<=> b a) = '>
;;; if (<=> a b) = '= then (<=> b a) = '=
;;; if (<=> a b) = #f then (<=> b a) = #f
;;; if (<=> a b) = '< and (<=> b c) = '< then (<=> a c) = '<
;;;
;;; Easily sort multi-valued elements by 1 entry, and if = then by a second entry,
;;; and if = then by a third entry and so on.
;;; Example use case with sorting:
#;
(begin
  (struct data (name quality quantity) #:prefab)
  (define data '(#s(data banana "sweet" 2)
                 #s(data apple "red" 4)
                 #s(data apple "red" 2)
                 #s(data kiwi "green" 2)
                 #s(data apple "green" 4)))
  
  (sort data (make-chain<=> symbol<=> data-name
                            string<=> data-quality
                            number<=> data-quantity
                            #:with-result order<?))
  ; -->
  #;
  (#s(data apple "green" 4)
   #s(data apple "red" 2)
   #s(data apple "red" 4)
   #s(data banana "sweet" 2)
   #s(data kiwi "green" 2)))
;;;
;;; Comparators can also be used to maintain a set of canonical elements (that is,
;;; when adding an element B in the set, if there is already an element A
;;; such that (<=> B A) is '<, then A should be removed).

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

;; Is c a possible return value of a comparator?
(define (order? c)
  (memq c '(< > = #f)))

;; Returns a comparator based on <=?.
;; Using <=? instead of <? enables returning #f when
;; neither a <=? b or b <=? a.
;; Particularly useful for numbers and +nan.0
(define (make<=> <=?)
  (λ (a b)
    (if (<=? a b)
      (if (<=? b a)
        '=
        '<)
      (if (<=? b a)
        '>
        #f))))

(define number<=> (make<=> <=))
(define string<=> (make<=> string<=?))
(define symbol<=> (make<=> (λ (a b) (or (eq? a b) (symbol<? a b)))))
(define   char<=> (make<=> char<=?))

(define (boolean<=> a b)
  #;'((#f #f =)
      (#f #t <)
      (#t #f >)
      (#t #t =))
  (if (eq? (and a #t) (and b #t))
    '=
    (if b '< '>)))

(module+ test
  (check<=> number<=> 1 2 '<)
  (check<=> number<=> 2 2 '=)
  (check<=> number<=> 2 2.0 '=)
  (check<=> number<=> +nan.0 +nan.0 #f)
  (check<=> number<=> 2 +nan.0 #f)
  (check<=> number<=> +inf.0 +inf.0 '=)
  (check<=> number<=> -inf.0 +inf.0 '<)
  
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

;; Curries op to return the <=>opposite of what
;; it would normally return.
;; cmp<=> may take more or less than 2 arguments, but for a binary comparator,
;; this amounts to swapping the order of the arguments, i.e.,
;; ((<=>swap cmp<=>) a b) = (cmp<=> b a)
;; Useful in particular with make-chain<=>
(define ((<=>swap cmp<=>) . args)
  (<=>opposite (apply cmp<=> args)))

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

;; Returns a procedure that takes two arguments and compares them
;; with-result : (-> order? any/c)
;;   Can be used for example with order<? to produce a boolean.
;; cmp+keys :  (list (list cmp<=> key) ...)
;;   cmp<=> : (-> T T order?)
;;   key : (-> any/c T)
;;   where T can be different from one cmp+key to the next.
;; For example, make-chain can be used with sort:
;; (sort l (make-chain<=> number<=> first
;;                        symbol<=> second
;;                        #:with-result order<?)
(define (make-chain<=> #:with-result [with-result values]
                       . cmp+keys)
  (unless (procedure-arity-includes? with-result 1)
    (raise-argument-error 'make-chain<=>
                          "with-result : Procedure of arity 1"
                          with-result))
  
  ;; Error checking
  (let loop ([cmp+keys cmp+keys])
      (cond [(empty? cmp+keys) (void)]
            [(empty? (rest cmp+keys))
             (raise-argument-error 'make-chain<=>
                                   "cmp+keys : An even number of cmp+keys"
                                   cmp+keys)]
            [else
             (define cmp (first cmp+keys))
             (define key (second cmp+keys))
             (unless (procedure-arity-includes? cmp 2)
               (raise-argument-error 'make-chain<=>
                                     "cmp<=> : Procedure of arity 2"
                                     cmp))
             (unless (procedure-arity-includes? key 1)
               (raise-argument-error 'make-chain<=>
                                     "key : Procedure of arity 1"
                                     key))
             (loop (rest (rest cmp+keys)))]))
  ;; The procedure to return
  (λ (g1 g2)
    (with-result
        (let loop ([cmp+keys cmp+keys])
          (cond [(empty? cmp+keys) '=]
                [else
                 (define cmp (first cmp+keys))
                 (define key (second cmp+keys))
                 (let ([v (cmp (key g1) (key g2))])
                   (case v
                     [(#f) #f] ; incomparable
                     [(=) (loop (rest (rest cmp+keys)))]
                     [else v]))])))))

(module+ test
  (let ([cmp<=> (make-chain<=> number<=> first
                               symbol<=> second)])
    (check-true (order>? (cmp<=> '(3 b) '(3 a))))
    (check-true (order>? (cmp<=> '(4 a) '(3 b))))
    (check-true (order=? (cmp<=> '(3 a) '(3 a)))))
  (let ([my-order>? (make-chain<=> #:with-result order>?
                                   number<=> first
                                   symbol<=> second)])
    (check-true  (my-order>? '(3 b) '(3 a)))
    (check-true  (my-order>? '(4 a) '(3 b)))
    (check-false (my-order>? '(3 a) '(3 a))))
  )

;; Given a sequence of comparators, returns a new comparator that
;; applies the first comparator and returns its value unless it is '=,
;; in which case it applies the second comparator and so on.
;; If the last comparator is applied and returns '=, then it is the return value.
;; The minimum number of comparisons is performed.
(define-syntax (chain-comparisons stx)
  (syntax-case stx ()
    [(_ cmp) #'cmp]
    [(_ cmp1 cmp2 ...)
     #'(let ([v cmp1])
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

  