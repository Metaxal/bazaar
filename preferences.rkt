#lang racket/base
;;; Copyright (C) Laurent Orseau, 2010-2013
;;; GNU Lesser General Public Licence (http://www.gnu.org/licenses/lgpl.html)

(require racket/dict
         racket/file
         racket/class
         racket/match
         )

(provide preferences%
         make-config-file-path
         )

;;; Simple preferences module to ensure forward-compatibility
;;; For a more sophisticated mechanism, see get-preferences in racket/file

; Use hash->list instead?
(define (hash->assoc-list h)
  (hash-map h (λ (k v)(cons k v))))

(define preferences%
  (class object% (super-new)
    (init-field file ; the file where to store the preferences
                [[preferences default-preferences]] ; an assoc list
                )
    
    (set! preferences
          (make-weak-hash preferences))
    
    (define/public (get-preference key)
      (dict-ref preferences key
                (λ _ (error "No preference value for " key))))
    
    (define/public (set-preference key val)
      (dict-set! preferences key val)
      (save-preferences)
      )
    
    (define/public (get-preferences)
      (hash->assoc-list preferences))
    
    (define/public  (save-preferences)
      (with-output-to-file file
        (λ()(write (get-preferences)))
        #:exists 'replace))
    
    (define/public (load-preferences)
      (set! preferences
            (let ([prefs
                   (if (file-exists? file)
                       (with-input-from-file file
                         (λ()(read)))
                       preferences)])
              ; if some value does not exist in the loaded preferences,
              ; take a default value from the default dictionary.
              ; This ensures forward compatibility, if a preference pair is added in a later version
              ; (also, if a value does not exist in the preferences, it is removed)
              (make-weak-hash
               (dict-map preferences
                         (λ(k v)(cons k (dict-ref prefs k v)))
                         ; v cannot be a procedure, since precedures are not saved to disk
                         ))
              )))
    
    
    (load-preferences)
    
    ))


;; Finds a config directory following XDG (X directory Guidelines):
;; http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html
;; First, look at $XDG_CONFIG_HOME. If it does not exist, use ~/.config directory.
;; If the directory does not exist, it is created.
;; (the ~/.my-software file or directory should not be used anymore)
(define (unix-config-dir)
  (let* ([xdg-config-home (getenv "XDG_CONFIG_HOME")]
         [home-dir (find-system-path 'home-dir)]
         [p (if xdg-config-home
                (build-path xdg-config-home)
                (build-path home-dir ".config"))])
    (unless (directory-exists? p)
      (make-directory* p))
    p))

;; Like (find-system-path 'home-dir), except that it uses (unix-config-dir) for unix.
;; dir-name must NOT be preceded by a ".". It will be added by find-config-path if necessary.
;; (it is added for windows and macos, but not for unix)

(define (find-config-path dir-name)
  (let ([dot-name (string-append "." dir-name)])
    (match (system-type 'os)
      [(or 'windows 'macos)
       (build-path (find-system-path 'home-dir) dot-name)]
      ;['macos   (find-system-path 'home-dir)]
      ['unix    (build-path (unix-config-dir) dir-name)])))

;; Returns a path to a config file.
;; If it does not exist, creates a directory in-dir in the config-dir, depending on the OS.
;; The file file is not created.
;; Under unix, the file is located in:
;; XDG_CONFIG_HOME/<in-dir>/<file> or in ~/.config/<in-dir>/<file>
;; Under windows and macos, it is located in:
;; (find-system-path 'home-dir)/.<in-dir>/<file>
(define (make-config-file-path in-dir file)
  (let ([p (find-config-path in-dir)])
    (unless (directory-exists? p)
      (make-directory* p))
    (build-path p file)))



#| Tests: | #
(define prefs (new preferences% 
                   [file (build-path (find-system-path 'home-dir) ".test-prefs")]
                   [default-preferences
                     '((a . "a") (b . "b"))]
                   ))

(send prefs get-preferences)
; '((b . "b") (a . "a"))
(send prefs set-preference 'a "aa")
(send prefs get-preferences)
; '((b . "b") (a . "aa"))

(define prefs2 (new preferences% 
                    [file (build-path (find-system-path 'home-dir) ".test-prefs")]
                    [default-preferences
                      '((a . "a") (c . "c"))]
                    ))
(send prefs2 get-preferences)
; '((a . "aa") (c . "c"))
; b has been removed, whereas c has got a default value

;|#

