#lang racket/base
(require (for-syntax racket/base
                     syntax/parse))

(provide with λ/with (rename-out [λ/with lambda/with]))

(define-syntax (with stx)
  (syntax-parse stx
    [(with it last)
     #'last]
    [(with it body1 body2 ...)
     #'(let ([it body1])
         (with it body2 ...))]))

(define-syntax-rule (λ/with it body ...)
  (λ (x) (with it x body ...)))

;;; Example
(module+ drracket
  (require racket)
  (with it
        "Hey\nThis is a nice little doggy you have here\nCome here doggy!\nNice doggy indeed\nSee ya!"
        (string-split it "\n")
        (filter (λ (line) (string-contains? line "doggy")) it)
        (sort it string<?)
        (remove-duplicates it))

  (define s
    "\
:black_large_square::black_large_square::large_yellow_square::large_yellow_square::black_large_square:
:large_yellow_square::black_large_square::black_large_square::large_yellow_square::black_large_square:
:black_large_square::black_large_square::large_yellow_square::black_large_square::black_large_square:
:large_green_square::large_green_square::large_green_square::large_green_square::large_green_square:")
  (map (λ/with it
               (string-replace it ":large_green_square:" "G" #:all? #t)
               (string-replace it ":black_large_square:" "B" #:all? #t)
               (string-replace it ":large_yellow_square:" "O" #:all? #t))
       (string-split s "\n")))