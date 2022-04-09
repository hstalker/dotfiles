;;; hgs-file.el --- Emacs file utilities -*- lexical-binding: t; -*-

;;; Commentary:

;; Provides useful file-related helpers

;;; Code:

(defmacro load-if-exists (file-path &optional nomessage nosuffix must-suffix)
  "Load file at `FILE-PATH' if it exists. Mainly just for clarity

This passes through the passed `NOSUFFIX' and `MUST-SUFFIX' to the underlying
load call as one might expect."
  `(load ,file-path 'noerror ,nomessage ,nosuffix ,must-suffix))

(provide 'hgs-file)

;; Local Variables:
;; mode: emacs-lisp
;; coding: utf-8-unix
;; fill-column: 80
;; require-final-newline: t
;; End:

;;; hgs-file.el ends here
