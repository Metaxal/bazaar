#lang racket/base
;;; Copyright (C) Laurent Orseau, 2010-2013
;;; GNU Lesser General Public Licence (http://www.gnu.org/licenses/lgpl.html)

(require racket/dict
         racket/file
         racket/class
         racket/match
         )

(provide preferences%
         make-pref-proc
         define-pref-procs
         make-config-file-path
         )

;;; Simple preferences module to ensure forward-compatibility
;;; For a more sophisticated mechanism, see get-preferences in racket/file

(define (pref-not-found key)
  (λ()(error "Preference not found: " key)))

;; TODO: turn that into a struct so as to make a prop:procedure ?
(define preferences%
  (class object% (super-new)
    (init-field file ; the file where to store the preferences
                ; Use make-config-file-path below as a convenience
                [[preferences default-preferences]] ; an assoc list
                [save-on-set? #f] ; if #t, preferences are saved each time set is called
                )
    (init [load? #t] ; if #f, prevents loading the preferences on initialization
          ) 
    
    (set! preferences
          (make-weak-hash preferences))
    
    (define/public (get key [default (pref-not-found key)])
      (dict-ref preferences key default))
    
    (define/public (set key val #:save? [save? save-on-set?])
      (dict-set! preferences key val)
      (when save? (save)))
    
    (define/public (set-save-on-set save?)
      (set! save-on-set? save?))
    
    ;; Returns the association list of preferences.
    ;; If sorted? is not #f, the keys are sorted with symbol<?
    (define/public (get-list #:sorted? [sorted? #f])
      (define a (hash->list preferences))
      (if sorted?
          (sort a symbol<? #:key car)
          a))
    
    ;; Returns the file used from which preferences are loaded from and saved to.
    (define/public (get-file)
      file)
    
    ;; pfile will be used subsequently to load and save the preferences,
    ;; but set-file does not trigger reading the preferences.
    ;; To both load and set the file, use load.
    (define/public (set-file pfile)
      (set! file pfile))
        
    ;; Loads or reloads the preferences from the file.
    ;; If file is #t, the same file is used
    ;; If file is #f, the preferences are not loaded
    ;; If file is a path-string?, this file is saved as the new basis for the preferences
    ;; (and a next load will also use this file)
    (define/public (load [pfile #t] #:clear? [clear? #f])
      (when clear? (clear))
      (when (and (path-string? pfile)
                 (not (file-exists? pfile)))
        (log-warning "Preference file ~a does not exist. Loading defaults."
                     pfile))
      (when pfile
        (let* ([pfile (if (eq? pfile #t) file pfile)]
               [prefs (if (and (path-string? pfile) 
                               (file-exists? pfile))
                          (file->value pfile)
                          '())]
               [prefs (if (or (eof-object? prefs)
                              (not (dict? prefs)))
                          '()
                          prefs)])
          (set-file pfile)
          ; if some value does not exist in the loaded preferences,
          ; take a default value from the default dictionary.
          ; This ensures forward compatibility, if a preference pair is 
          ; added in a later version.
          ; All other values loaded from the file are kept unchanged.
          (for ([(k v) (in-dict prefs)])
            (dict-set! preferences k v))
          )))

    ;; Clears the preferences (empty dictionary)
    (define/public (clear)
      (hash-clear! preferences))

    ;; Save the preferences to the given file
    (define/public  (save [pfile file])
      (when pfile
        (with-output-to-file pfile
          (λ()(write (get-list)))
          #:exists 'replace)))
    
    (when load? (load))
    
    ))

;; Returns a procedure similar to a parameter:
;; if no argument is provided, returns the preference,
;; Otherwise sets the preference to the given value.
;; Warning: It uses the value of prefs at the time of the definition, so if prefs
;; is changed afterwrads, it won't see it.
(define (make-pref-proc prefs key)
  (case-lambda
    [() (send prefs get key (pref-not-found key))]
    [(val) (send prefs set key val)]
    [(val save?) (send prefs set key val #:save? save?)]))

;; Helper to avoid using define-values and not have procs in front of their keys.
;; Warning: It uses the value of prefs at the time of the definition, so if prefs
;; is changed afterwrads, it won't see it.
(define-syntax-rule (define-pref-procs prefs
                      [proc key args ...] ...)
  (begin
    (define proc (make-pref-proc prefs key args ...))
    ...))


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


(module+ test
  (require rackunit)
  
  (define f (make-temporary-file))
  
  (define prefs (new preferences% [file f]
                     [default-preferences
                       '((a . "a") (b . "b"))]))
  
  (define (get/sort prefs)
    (send prefs get-list #:sorted? #t))
  
  (check-equal? (get/sort prefs)
                '((a . "a") (b . "b")))
  
  (define-pref-procs prefs
    [a 'a]
    [b 'b])
  (check-equal? (a) "a")
  (check-equal? (b) "b")
  (a "aaa")
  (check-equal? (a) "aaa")
  (a "a")
  (check-equal? (a) "a")
  
  (send prefs set 'a "aa") ; and save to file
  (check-equal? (get/sort prefs)
                '((a . "aa") (b . "b")))
  
  ; load the preferences. 'a is loaded, but 'b was not saved, so not loaded.
  (define prefs2 (new preferences% [file f]
                      [default-preferences
                        '((a . "a") (c . "c"))]))
  
  ; New key with default value.
  ; Check also that b has not been removed
  (check-equal? (get/sort prefs2)
                '((a . "aa") (b . "b") (c . "c")))
  
  (send prefs2 save)
  
  (define prefs3 (new preferences% [file f]
                      [default-preferences '((a . "a")(b . "bb"))]))
  (check-equal? (get/sort prefs2)
                '((a . "aa") (b . "b") (c . "c")))
  
  )
