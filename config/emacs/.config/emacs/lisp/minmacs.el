;;; minmacs.el --- Declarative packaging primitives -*- lexical-binding: t; -*-

;;; Commentary:

;; Provides some core functions and macros layered on top of straight.el
;; facilities useful for declaratively specifying a package setup.

;;; Code:

(defgroup minmacs nil
  "Options for our declarative package management & system management
wrappers."
  :prefix "minmacs-"
  :group 'local
  :version "26.1")

;; Define functions for somewhat declaratively managing packages
(defvar minmacs--base-packages
  '(use-package
    bind-key)
  "Base set of packages to install immediately when finished bootstrapping
straight.el")

(defvar bootstrap-version)
(defvar straight-base-dir
  (file-name-as-directory hgs-data-directory))
(defvar straight-repository-branch
  "master"
  "Use master instead of develop for stability.")
(defvar straight-vc-git-default-protocol
  'https
  "Prefer https over alternative protocols.")
(defvar straight-vc-git-default-clone-depth
  1
  "Save some space (may break a few weird packages).")
(defvar straight-check-for-modifications
  '(check-on-save)
  "Reduce startup time.")
(defvar straight-use-package-by-default
  nil
  "We don't want use-package integration by default.")

(defun minmacs-bootstrap ()
  "Bootstrap necessary package management libraries."
  (let* ((bootstrap-file (concat straight-base-dir
                                 "straight/repos/straight.el/bootstrap.el"))
         (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
        (url-retrieve-synchronously
          "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
          'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))
  (dolist (pkg minmacs--base-packages)
    (straight-use-package pkg)))

(provide 'minmacs)

;; Local Variables:
;; coding: utf-8-unix
;; fill-column: 80
;; require-final-newline: t
;; End:

;;; minmacs ends here
