;;; hgs-pre-init.el --- Pre-init functionality -*- lexical-binding: t; -*-

;;; Commentary:

;; Provides code to be used prior to main init procedures. This is the best
;; place to put functionality shared between early-init.el and init.el.

;;; Code:

(require 'hgs-core)

;; comp is only available with native-comp
(unless (version< emacs-version "28")
  (require 'comp))
(require 'package)

(defun hgs-run-pre-init ()
  ;; Defer garbage collection until end of initialization
  (customize-set-variable 'gc-cons-threshold most-positive-fixnum)

  ;; Needed to force emacs to not use stale bytecode
  (customize-set-variable 'load-prefer-newer t)

  (if (fboundp 'json-serialize)
      (message "Native JSON is available.")
    (message "Native JSON is not available."))

  (if (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
      (progn
        (message "Native compilation is available.")

        ;; Redirect cache to to my cache directory
        (when (fboundp 'startup-redirect-eln-cache)
          (startup-redirect-eln-cache
           (expand-file-name hgs-emacs-cache-directory)))

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

  ;; Hide GUI emacs remnants early
  (push '(menu-bar-lines . nil) default-frame-alist)
  (push '(tool-bar-lines . nil) default-frame-alist)
  (push '(vertical-scroll-bars . nil) default-frame-alist)
  (push '(horizontal-scroll-bars . nil) default-frame-alist)

  ;; We don't want to use xresources for themes
  (advice-add #'x-apply-session-resources :override #'ignore)

  ;; GTK Emacs will react to gconf settings for things like fonts by default. This
  ;; turns that off.
  (define-key special-event-map [config-changed-event] 'ignore))

(provide 'hgs-pre-init)

;; Local Variables:
;; mode: emacs-lisp
;; coding: utf-8-unix
;; fill-column: 80
;; require-final-newline: t
;; End:

;;; hgs-pre-init.el ends here
