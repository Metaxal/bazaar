#lang racket/base
;;; Copyright (C) Laurent Orseau, 2010-2013
;;; GNU Lesser General Public Licence (http://www.gnu.org/licenses/lgpl.html)

(require "list.rkt"
         "mutation.rkt"
         racket/list
         racket/function)

(provide (all-defined-out))

; Tree example : '(root (node1 leaf1 (node2 leaf2 leaf3) leaf4) leaf5 (node leaf6 leaf7))

; number of nodes+leaves of a tree:
(define (tree-length tree) (length (flatten tree)))

(define (build-bin-tree n)
  (if (= 0 n)
      (list n n n)
      (list n n (build-bin-tree (- n 1)))))


; Depth-first iteration
(define (tree-map tree leaf-proc [node-proc identity])
  (let loop ([tree tree])
    (if (list? tree)
      (cons (node-proc (loop (first tree)))
            (map (λ(t)(loop t))
                 (rest tree)))
      (leaf-proc tree))))

;; node-proc: (not/c list?) int -> any/c
;; leaf-proc: (not/c list?) int -> any/c
;; The second argument is the depth, which is 0 for the root.
(define (tree-map/depth tree leaf-proc [node-proc identity])
  (let loop ([tree tree] [depth 0])
    (if (list? tree)
        (map (λ(t)(node-proc (loop t (add1 depth))))
             tree)
        (leaf-proc tree depth))))

(define (tree-for-each tree leaf-proc [node-proc leaf-proc])
  (let loop ([tree tree])
    (if (list? tree)
        (begin (node-proc tree)
               (for-each loop tree))
        (void (leaf-proc tree)))))

; like fold but for trees, where the initial values are leaf values (modified by leaf-proc)
; node-proc accumulates the children return values
; tree-fold : tree . node-proc . node-value -> return-value
; node-proc : node-value . (list return-value) -> return-value
; leaf-proc : leaf-value -> return-value
(define (tree-fold tree node-proc [leaf-proc values])
  (if (list? tree)
      (node-proc (first tree) (map (λ(tree)(tree-fold tree node-proc leaf-proc))
                                   (rest tree)))
      (leaf-proc tree)))

(define (subtree tree n)
  (first
   (let loop ([tree tree])
     (-- n)
     (cond [(= -1 n) (list tree)]
           [(list? tree) (append-map loop (rest tree))]
           [else '()]))))

(define (tree-set tree n sub)
  (let loop ([tree tree])
    (-- n)
    (cond [(= -1 n) sub]
          [(list? tree) (cons (first tree) (map loop (rest tree)))]
          [else tree])))

(define (tree-choose tree)
  (subtree tree (random (tree-length tree))))

(define (tree->num-tree tree)
  (let ([n -1])
    (let loop ([tree tree])
      (++ n)
      (cond [(list? tree) (apply list n (map loop (rest tree)))]
            [else n]))))

(define (exchange-choose-subtrees tree)
  ; exchanges randomly two subtrees of tree
  ; the root cannot be chosen for exchange!
  (let* ([n1 (+ (random (- (tree-length tree) 1)) 1)]
         [numtree (tree->num-tree tree)]
         [n1list (flatten (subtree numtree n1))]
         [tree-rest
          (filter 
           values
           (build-list
            (tree-length tree)
            (λ(n)(if (or 
                      (member n1 (flatten (subtree numtree n)))
                      (member n n1list))
                     #f
                     n))))]
         [n2 (choose tree-rest)]
         [subtree1 (subtree tree n1)]
         [subtree2 (subtree tree n2)]
         )
    (let loop ([tree tree]
               [ntree numtree])
      (if (list? tree)
          (cond [(= (first ntree) n1) subtree2]
                [(= (first ntree) n2) subtree1]
                [else 
                 (cons (first tree) 
                       (map loop (rest tree) (rest ntree)))])
          (cond [(= ntree n1) subtree2]
                [(= ntree n2) subtree1]
                [else tree])))))
          

;TESTS:
;(define tree1
;  '(root (node1 leaf1 (node2 leaf2 leaf3) leaf4) leaf5 (node leaf6 leaf7)))
;> (subtree tree1 3)
;(node2 leaf2 leaf3)
;> (subtree tree1 4)
;leaf2
;> (tree-set tree1 3 'leaf8)
;(root (node1 leaf1 leaf8 leaf4) leaf5 (node leaf6 leaf7))
;> (tree-map tree1 symbol->string identity)
