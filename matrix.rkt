#lang racket/base

;;; 2d matrices
;;; I wish I'd call this `board` instead...

(require define2
         racket/performance-hint
         racket/list
         racket/vector
         )

(provide (rename-out [create-matrix make-matrix])
         (struct-out matrix)
         matrix-copy
         matrix-copy!
         matrix-fill!
         ll->matrix
         matrix->ll
         matrix-as-vector
         matrix-ref
         matrix-ref-cycle
         matrix-set!
         matrix-update!
         matrix-map!
         matrix-for-each
         matrix-in-bounds?
         )

(struct matrix 
  (nrows ncols mat)
  #:property prop:sequence
  (lambda (mx)
    (let ([nrow (matrix-nrows mx)]
          [ncol (matrix-ncols mx)])
      (make-do-sequence
       (lambda ()
         (values (λ(pos)(let ([row (first pos)]  ; pos -> element ; pos=(row col)
                              [col (second pos)])
                          ;(printf "row: ~a col:~a \n" row col)
                          (values row col (matrix-ref mx row col))))
                 (λ(pos)(let ([row (first pos)] ; pos -> next-pos
                              [col (second pos)])
                          (if (= col (- ncol 1))
                              (list (add1 row) 0)
                              (list row (add1 col)))))
                 '(0 0) ; initial pos
                 (λ(pos)(< (first pos) nrow)) ; existing position ?
                 (lambda _ #t)
                 (lambda _ #t)))))))

(begin-encourage-inline

;; Fills to-mat with the elements of mat.
;; to-mat must be the same size as mat (and we should check)
(define (matrix-copy! to-mat mat)
  (vector-copy! (matrix-mat to-mat) 0 (matrix-mat mat)))

(define (matrix-copy mat)
  (matrix (matrix-nrows mat) (matrix-ncols mat)
               (vector-copy (matrix-mat mat))))

(define (matrix-fill! mat val)
  (vector-fill! (matrix-mat mat) val))


(define (matrix-as-vector m) ;:-> vector?
  ;: Returns the matrix $m as a linear vector.
  (matrix-mat m))

(define (create-matrix nrows [ncols nrows] [val 0])
  (matrix nrows ncols 
               (make-vector (* nrows ncols) val)))

(define (ll->matrix ll)
  (matrix (length ll) (length (first ll))
                (list->vector (apply append ll))))

(define (matrix->ll mat)
  (for/list ([i (in-range (matrix-nrows mat))])
    (for/list ([j (in-range (matrix-ncols mat))])
      (matrix-ref mat i j))))

(define (matrix-in-bounds? mat row col)
  (and (<= 0 row (- (matrix-nrows mat) 1))
       (<= 0 col (- (matrix-ncols mat) 1))))

(define (coord->index row col row-size)
  (+ (* row row-size) col))

(define (matrix-ref mat row col)  
  (vector-ref (matrix-mat mat) (coord->index row col (matrix-ncols mat))))

(define (matrix-set! mat row col val)  
  (vector-set! (matrix-mat mat) (coord->index row col (matrix-ncols mat)) val))

(define (matrix-update! mat row col updater)
  (let ([coord (coord->index row col (matrix-ncols mat))]
        [m (matrix-mat mat)])
    (vector-set! m coord  
                 (updater (vector-ref m coord)))))


(define (matrix-map! mat proc)
  (for* ([i (in-range (matrix-nrows mat))]
         [j (in-range (matrix-ncols mat))])
;  (ntimes [i (matrix-nrows mat)]
;          (ntimes [j (matrix-ncols mat)]
    (matrix-set! mat i j (proc i j (matrix-ref mat i j)))))

;; proc: i j m[i,j] -> any
(define (matrix-for-each mat proc)
  (for* ([i (in-range (matrix-nrows mat))]
         [j (in-range (matrix-ncols mat))])
    (proc i j (matrix-ref mat i j))))

(define (matrix-ref-cycle mat r c)  
  (let ([rows (matrix-nrows mat)]        
        [cols (matrix-ncols mat)])    
    (let ([r (cond [(< r 0) (+ r rows)]                   
                   [(>= r rows) (- r rows)]                   
                   [else r])]          
          [c (cond [(< c 0) (+ c cols)]                   
                   [(>= c cols) (- c cols)]                   
                   [else c])])
      (matrix-ref mat r c))))

) ; end encourage inline


;=============;
;=== Tests ===;
;=============;

(module+ test
  (require rackunit)
  
  (define mx (create-matrix 3 5))

  (check-true (matrix-in-bounds? mx 0 0))
  (check-true (matrix-in-bounds? mx 2 4))
  (check-false (matrix-in-bounds? mx 2 5))
  (check-false (matrix-in-bounds? mx 3 4))
  (check-false (matrix-in-bounds? mx -1 0))
  (check-false (matrix-in-bounds? mx 0 -1))
  
  (for ([(row col val) mx])
    (matrix-set! mx row col (* row col)))

  (check-equal? (matrix->ll mx)
                '((0 0 0 0 0) (0 1 2 3 4) (0 2 4 6 8)))

  (define mx2 (matrix-copy mx))
  (matrix-set! mx2 0 1 1000)

  (check-equal? (matrix->ll mx2)
                '((0 1000 0 0 0) (0 1 2 3 4) (0 2 4 6 8)))

  (check-equal? (matrix->ll mx)
                '((0 0 0 0 0) (0 1 2 3 4) (0 2 4 6 8))) ; no change

  (matrix-copy! mx mx2)

  (check-equal? (matrix->ll mx)
                '((0 1000 0 0 0) (0 1 2 3 4) (0 2 4 6 8)))

  (check-equal? (matrix->ll mx2)
                '((0 1000 0 0 0) (0 1 2 3 4) (0 2 4 6 8)))
  
  (matrix-fill! mx 'a)
  
  (check-equal? (matrix->ll mx)
                '((a a a a a) (a a a a a) (a a a a a)))

  (check-equal? (matrix->ll mx2)
                '((0 1000 0 0 0) (0 1 2 3 4) (0 2 4 6 8))) ; no change
  )
