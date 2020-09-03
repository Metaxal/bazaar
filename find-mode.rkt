#lang racket/base

(require racket/list
         racket/dict
         racket/contract)
(provide
 (contract-out
  ; Main function with all the features
  [find-mode
   (-> (procedure-arity-includes/c 1)
       real? real?
       (procedure-arity-includes/c 2)
       #:ε (or/c #f positive?)
       #:fε (or/c #f positive?)
       #:n (or/c #f exact-nonnegative-integer?)
       list?)]
  ; Convenience, simpler functions
  [minimize
   (-> (procedure-arity-includes/c 1)
       real? real?
       positive?
       (list/c real? real?))]
  [maximize
   (-> (procedure-arity-includes/c 1)
       real? real?
       positive?
       (list/c real? real?))]))

; We could also use α=2/3 for example, but then we would need to query two points per step to ensure
; a reduction of 1/4.
; The golden ratio allows to get only one function evaluation per step by reusing the previous point
; (By querying two points per step, at best we would have a reduction of 1/2, but after
; 2 steps with one query each, with the golden ratio ensures a reduction of 0.618…, which is better)
(define α (/ (- (sqrt 5.) 1.) 2.)) ; 0.618…
(define 1-α (- 1. α)) ; 0.382…
(define (between/closer x y) (+ (* α x) (* 1-α y))) ; between x and y but closer to x

;; Finds the mode of a unimodal function on the interval [a, b], within x-precision ε.
;; Returns the list of the minimum and its f-value.
;; The number of queries to f is exactly max{4, ⌈log_ø (b-a)/ε⌉ } where ø=(3+√5)/2
;; (although the precision is actually ε×0.382).
;; https://en.wikipedia.org/wiki/Golden-section_search
;; f : function to optimize (assumed unimodal)
;; a : x-min
;; b : x-max
;; better? : Usually < or >
;; ε : x-precision
;; fε : y-precision
;; n : maximum number of iterations
;; Returns a dictionary.
;; TODO: Check if non-unimodal (in some cases)?
(define (find-mode f a b better?
                   #:ε [ε #f]
                   #:fε [fε #f]
                   #:n [nmax #f])
  (unless (or ε fε nmax)
    (error
     'find-mode
     "at least one of ε, fε and nmax must be non-#f"))
  (define c (between/closer a b))
  (define d (between/closer b a))
  (define-values (fa fb fc fd) (apply values (map f (list a b c d))))
  (let loop ([a a] [c c] [d d] [b b] [fa fa] [fc fc] [fd fd] [fb fb] [n 0])
    #;(debug-vars a b fa fb)
    (define fεx (abs (- fa fb)))
    (define εx (- b a))
    (cond
      [(or (and fε (<= fεx fε))
           (and ε (<= εx ε))
           (and nmax (>= n nmax)))
       (define l (list (list a fa) (list c fc) (list d fd) (list b fb)))
       (define best (argmin second l))
       (list (cons 'x (first best))
             (cons 'fx (second best))
             (cons 'n n)
             (cons 'n-eval (+ n 4))
             (cons 'ε εx)
             (cons 'fε fεx)
             (cons 'last-points l))]
      [else
       (if (better? fc fd)
           (let ([e (between/closer a d)]) (loop a e c d fa (f e) fc fd (+ n 1)))
           (let ([e (between/closer b c)]) (loop c d e b fc fd (f e) fb (+ n 1))))])))

(define (minimize f a b ε)
  (define res (find-mode f a b < #:ε ε))
  (list (dict-ref res 'x) (dict-ref res 'fx)))

(define (maximize f a b ε)
  (define res (find-mode f a b > #:ε ε))
  (list (dict-ref res 'x) (dict-ref res 'fx)))

(module+ test
  (require racket/math
           racket/dict
           rackunit)
  
  (let* ([x* 3.5]
         [f (λ (x) (+ 10 (sqr (- x x*))))]
         [fx* (f x*)]
         [ε 0.0001]
         [fε 0.0000001]
         [nmax 10])
    
    (let ([res (find-mode f 0 10 < #:ε ε)])
      (check-= (dict-ref res 'x) x* ε)
      (check <= (dict-ref res 'ε) ε)
      (check-= (dict-ref res 'x) x* (dict-ref res 'ε)))
  
    (let ([res (find-mode f 0 10 < #:fε fε)])
      (check-= (dict-ref res 'fx) fx* fε)
      (check <= (dict-ref res 'fε) fε)
      (check-= (dict-ref res 'fx) fx* (dict-ref res 'fε)))

    (let ([res (find-mode f 0 10 < #:n nmax)])
      (check-equal? (dict-ref res 'n) nmax)))

  (let ([res (maximize sin -3 3 0.001)])
    (check-= (first res) (/ pi 2) 0.001))

  )
