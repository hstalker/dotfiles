;;; early-init.el --- Earliest load stage -*- lexical-binding: t; -*-

;;; Commentary:

;; Runs before init in the startup process, allowing us to improve performance
;; and prevent graphical oddities earlier.

;;; Code:

(let ((default-directory (concat (file-name-directory load-file-name)
                                 "lisp")))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; comp is only available with native-comp
(unless (version< emacs-version "28")
  (require 'comp))
(require 'package)

;; Defer garbage collection until end of initialization
(defvar default-gc-cons-threshold (* 1024 1024 128)) ;; 128 MiB
(customize-set-variable 'gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
          (lambda (&rest _)
            "Reset GC settings to defaults."
            (customize-set-variable 'gc-cons-threshold default-gc-cons-threshold)
            (customize-set-variable 'gc-cons-percentage 0.1)
            (makunbound 'default-gc-cons-threshold)))

;; Disable file-name handling during startup
(defvar default-file-name-handler-alist file-name-handler-alist)
(customize-set-variable 'file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda (&rest _)
            "Reset file name handler alist to defaults."
            (customize-set-variable 'file-name-handler-alist default-file-name-handler-alist)
            (makunbound 'default-file-name-handler-alist)))

;; Needed to force emacs to not use stale bytecode
(customize-set-variable 'load-prefer-newer t)

(if (fboundp 'json-serialize)
    (message "Native JSON is available.")
  (message "Native JSON is not available."))

(if (and (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    (progn
      (message "Native compilation is available.")
      ;; Log async native-compilation warnings, but don't pop up the window
      (customize-set-variable 'native-comp-async-report-warnings-errors
                              'silent)
      ;; Enable deferred compilation by default, reasonably early
      (when (boundp 'native-comp-deferred-compilation)
        (customize-set-variable 'native-comp-deferred-compilation t)))
  (message "Native compilation is not available."))

;; Stop package.el from starting by default
(customize-set-variable 'package-enable-at-startup nil)
(declare-function package--ensure-init-file "package")
(advice-add #'package--ensure-init-file :override #'ignore)

;; Avoid displaying too much stuff after startup
(customize-set-variable 'inhibit-startup-message t)
(customize-set-variable 'inhibit-startup-echo-area-message nil)
(customize-set-variable 'inhibit-default-init t)
(customize-set-variable 'initial-scratch-message nil)

;; Avoid causing resizing the frame during startup
(customize-set-variable 'frame-inhibit-implied-resize t)
(customize-set-variable 'frame-resize-pixelwise t)

;; Prevents redundant reloading of fonts during redisplay
(setq inhibit-compacting-font-caches t)

;; Hide GUI emacs remnants early
(push '(tab-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(push '(horizontal-scroll-bars . nil) default-frame-alist)

;; We don't want to use xresources for themes
(advice-add #'x-apply-session-resources :override #'ignore)

;; GTK Emacs will react to gconf settings for things like fonts by default. This
;; turns that off.
(define-key special-event-map [config-changed-event] 'ignore)

;; Improves latency with child-frames
(when (fboundp 'pgtk-wait-for-event-timeout)
  (setq pgtk-wait-for-event-timeout 0.001))

;; Unneeded
;; (provide 'early-init)

;; Local Variables:
;; coding: utf-8-unix
;; fill-column: 80
;; require-final-newline: t
;; no-native-compile: nil
;; no-byte-compile: nil
;; End:

;;; early-init.el ends here
