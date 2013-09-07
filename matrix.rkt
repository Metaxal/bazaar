#lang racket/base
;;; Copyright (C) Laurent Orseau, 2010-2013
;;; GNU Lesser General Public Licence (http://www.gnu.org/licenses/lgpl.html)


(require racket/performance-hint
         racket/list
         racket/vector
         )

(provide (rename-out [create-matrix make-matrix])
         matrix?
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
         matrix-nrows
         matrix-ncols
         )

(define-struct matrix 
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

(define (matrix-copy mat [to-mat #f])
  (make-matrix (matrix-nrows mat) (matrix-ncols mat)
               (vector-copy (matrix-mat mat))))

(define (matrix-fill! mat val)
  (vector-fill! (matrix-mat mat) val))


(define (matrix-as-vector m) ;:-> vector?
  ;: Returns the matrix $m as a linear vector.
  (matrix-mat m))

(define (create-matrix nrows [ncols nrows] [val 0])
  (make-matrix nrows ncols 
               (make-vector (* nrows ncols) val)))

(define (ll->matrix ll)
  (make-matrix (length ll) (length (first ll))
                (list->vector (apply append ll))))

(define (matrix->ll mat)
  (for/list ([i (in-range (matrix-nrows mat))])
    (for/list ([j (in-range (matrix-ncols mat))])
      (matrix-ref mat i j))))

(define-inline (coord->index row col row-size)
  (+ (* row row-size) col))

(define-inline (matrix-ref mat row col)  
  (vector-ref (matrix-mat mat) (coord->index row col (matrix-ncols mat))))

(define-inline (matrix-set! mat row col val)  
  (vector-set! (matrix-mat mat) (coord->index row col (matrix-ncols mat)) val))

(define-inline (matrix-update! mat row col updater)
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


(define (init-random mat)  
  (matrix-map! mat (λ(i j v) 0)))

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

#| Tests | #

(define mx (create-matrix 3 5))
(for ([(row col val) mx])
  (matrix-set! mx row col (* row col)))

(define mx2 (matrix-copy mx))
(matrix-set! mx2 0 0 1000)

(matrix->ll mx)
(matrix->ll mx2)

(matrix-copy! mx mx2)

(matrix->ll mx)
(matrix->ll mx2)

(matrix-fill! mx 'a)
(matrix->ll mx)

;(for/list ([(row col val) mx])
;  (list row col val))

;(for/list ([(row col val) mx])
;  (list row col val))


;|#


