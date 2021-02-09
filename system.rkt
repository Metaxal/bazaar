#lang racket/base

(require racket/file)

(provide (all-defined-out))

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
    (case (system-type 'os)
      [(windows macos)
       (build-path (find-system-path 'home-dir) dot-name)]
      ;['macos   (find-system-path 'home-dir)]
      [(unix)    (build-path (unix-config-dir) dir-name)])))
