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
;; Satisfy the Emacs compiler
(defvar hgs-data-directory)
(defvar bootstrap-version)

;; Configure Straight
(defvar straight-base-dir
  (file-name-as-directory hgs-data-directory))
(defvar straight-repository-branch
  "master"
  "Use master instead of develop for stability.")
(defvar straight-vc-git-default-protocol
  ;; We should eventually consider moving to ssh
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
  (let ((bootstrap-progress
         (make-progress-reporter "Bootstrapping minmacs..." 0 2)))
    (let* ((bootstrap-file (concat straight-base-dir
                                   "straight/repos/straight.el/bootstrap.el"))
           (bootstrap-version 5))
      (unless (file-exists-p bootstrap-file)
        (with-current-buffer
            (url-retrieve-synchronously
             (let ((protocol "https")
                   (repo "raxod502/straight.el")
                   (branch "develop"))
               (format
                "%s://raw.githubusercontent.com/%s/%s/install.el"
                protocol repo branch))
             'silent 'inhibit-cookies)
          (goto-char (point-max))
          (eval-print-last-sexp)))
      (load bootstrap-file nil 'nomessage))
    (progress-reporter-update bootstrap-progress 1)
    (declare-function straight-use-package "straight")
    (straight-use-package
     '(use-package
        :type git
        :host github
        :repo "jwiegley/use-package"))
    (require 'use-package)
    (progress-reporter-done bootstrap-progress)))

(provide 'minmacs)

;; Local Variables:
;; coding: utf-8-unix
;; fill-column: 80
;; require-final-newline: t
;; End:

;;; minmacs ends here
