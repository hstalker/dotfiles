;;; hgs-post-init.el --- Post-init functionality -*- lexical-binding: t; -*-

;;; Commentary:

;; Provides code to be used after main init procedures. This is the best place
;; to put functionality for undoing performance enhancement during init.

;;; Code:

(require 'hgs-core)

(defun hgs-run-post-init ()
  ;; For non daemon run we want to manually run frame setup hooks that have been
  ;; configured, as init file runs after the initial frame is created.
  (unless (daemonp)
    (hgs--new-frame-setup))

  ;; Reset GC threshold to 100MB
  (customize-set-variable 'gc-cons-threshold (* 1024 1024 100)))

(provide 'hgs-post-init)

;; Local Variables:
;; mode: emacs-lisp
;; coding: utf-8-unix
;; fill-column: 80
;; require-final-newline: t
;; End:

;;; hgs-post-init.el ends here
