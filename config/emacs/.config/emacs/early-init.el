;;; early-init.el --- Run before init -*- lexical-binding: t; -*-

;;; Commentary:

;; Runs before init in the startup process, allowing us to improve performance
;; and prevent graphical oddities earlier.

;;; Code:

(let ((default-directory (concat (file-name-directory load-file-name)
                                 "lisp")))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))
(require 'hgs-pre-init)

(hgs-run-pre-init)

;; Local Variables:
;; coding: utf-8-unix
;; fill-column: 80
;; require-final-newline: t
;; no-native-compile: nil
;; End:

;;; early-init.el ends here
