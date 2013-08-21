#lang slideshow
;;; Copyright (C) Laurent Orseau, 2010-2013
;;; GNU Lesser General Public Licence (http://www.gnu.org/licenses/lgpl.html)

(require "../slideshow-toc.rkt")

; Some layout for the toc:
(define old-slide-assembler (current-slide-assembler))
(current-slide-assembler
 (λ(str i p)
   (lt-superimpose
    (old-slide-assembler str i p)
    (parameterize [(current-font-size 12)]
      (frame
       (apply vl-append
              (for/list ([section (get-table-of-contents)]
                         [i (in-naturals)])
                (if (= i current-section-num)
                    (vl-append
                     (colorize (t (first section)) "black")
                     (hc-append
                      (blank gap-size 0)
                      (apply vl-append
                             (blank)
                             (for/list ([sub (rest section)]
                                        [j (in-naturals)])
                               (colorize (t sub)
                                         (if (and (= i current-section-num)
                                                  (= j current-subsection-num))
                                             "blue" "gray"))))))
                    ; else
                    (colorize (t (first section)) "gray")
                    ))))))))

; Display a slide automatically each time a section/slide is called
(current-slide-section
 (λ(title)
   (slide #:title (string-append "Part " (number->string (add1 current-section-num)))
          (bit title))))

(section/slide "Section 1")
(slide (t "First section"))

(subsection/slide "Subsection 1-1")
(slide (t "1-1-1"))
(slide (t "1-1-2"))
(subsection/slide "Subsection 1-2")
(slide (t "1-2-1"))
(slide (t "1-2-2"))

(section/slide "Section 2")
(slide (t "Second section"))

; remove the automatic section slides
(current-slide-section #f)
; adding automatic subsection slides
(current-slide-subsection
 (λ(title)
   (slide #:title "This is subsection..."
          (frame (it (format "~a / ~a" (add1 current-section-num)
                             (add1 current-subsection-num)))))))

(section/slide "Section 3")
(slide (t "Third section"))
(subsection/slide "Subsection 3-1")
(slide (t "3-1-1"))
(slide (t "3-1-2"))
(subsection/slide "Subsection 3-2")
(slide (t "3-2-1"))
(slide (t "3-2-2"))

(build-table-of-contents get-table-of-contents)

