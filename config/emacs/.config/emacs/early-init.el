;;; early-init.el --- Run before init -*- lexical-binding: t; -*-

;;; Commentary:

;; Runs before init in the startup process, allowing us to improve performance
;; and prevent graphical oddities earlier.

;;; Code:

;; Defer garbage collection until end of initialization
(customize-set-variable 'gc-cons-threshold most-positive-fixnum)

;; Needed to force emacs to not use stale bytecode
(customize-set-variable 'load-prefer-newer t)

;; Stop package.el from starting by default
(customize-set-variable 'package-enable-at-startup nil)
(declare-function package--ensure-init-file "package")
(advice-add #'package--ensure-init-file :override #'ignore)

;; Hide GUI emacs remnants early
(push '(menu-bar-lines . nil) default-frame-alist)
(push '(tool-bar-lines . nil) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(push '(horizontal-scroll-bars . nil) default-frame-alist)

;; We don't want to use xresources for themes
(advice-add #'x-apply-session-resources :override #'ignore)

;; GTK Emacs will react to gconf settings for things like fonts by default. This
;; turns that off.
(define-key special-event-map [config-changed-event] 'ignore)

;; Local Variables:
;; coding: utf-8-unix
;; fill-column: 80
;; require-final-newline: t
;; End:

;;; early-init.el ends here
