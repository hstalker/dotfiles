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
(defvar straight-base-dir hgs-data-directory)
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
(defvar straight-fix-flycheck
  t
  "Install a workaround for a problem with flycheck.")
(defvar straight-fix-org
  t
  "Install a workaround for a problem with org.")
(defvar straight-profiles
  `((core . ,(concat hgs-config-directory "core-lock.el"))
    (custom . ,(concat hgs-config-directory "custom-lock.el")))
  "Alist of `'(PROFILE-NAME . LOCK-FILE-NAME)'.
Lock file names are either relative to \"straight/versions/\" or absolute
paths.")
(defvar straight-current-profile
  'core
  "Current profile.
Should match an entry in `straight-profiles'. Bind this to different values over
different parts of `core-package.el' in order to have that profile be in
effect.")

(defmacro minmacs--with-no-default-repositories (&rest body)
  "Disable default ELPA/MELPA etc. repositories for `BODY'."
  (declare (indent defun))
  `(let ((straight-recipe-repositories nil)
         (straight-recipe-overrides nil))
     ,@body))

(defmacro minmacs-as-core-packages (&rest body)
  "Make all package declarations inside `BODY' be specified as core packages."
  (declare (indent defun))
  `(minmacs--with-no-default-repositories
     (let ((straight-current-profile 'core))
       ,@body)))

(defmacro minmacs-as-custom-packages (&rest body)
  "Make all package declarations inside `BODY' be specified as custom packages."
  (declare (indent defun))
  `(let ((straight-current-profile 'custom))
     ,@body))

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
    ;; (customize-set-variable
    ;;  'straight-recipe-repositories
    ;;  nil
    ;;  "Default repositories to use for recipe-less package
    ;; declarations. We don't want to use things like MELPA by
    ;; default, and much prefer to manually specify ALL dependencies
    ;; in our configuration in order to be maximally reproducible.")
    ;; (customize-set-variable 'straight-recipe-overrides nil)
    (declare-function straight-use-package "straight")
    (minmacs-as-core-packages
      (straight-use-package
       '(diminish
         :type git
         :host github
         :repo "jwiegley/use-package"))
      (straight-use-package
       '(bind-key
         :type git
         :host github
         :repo "jwiegley/use-package"))
      (straight-use-package
       '(use-package
          :type git
          :host github
          :repo "jwiegley/use-package")))
    (require 'use-package)
    (progress-reporter-done bootstrap-progress)))

(provide 'minmacs)

;; Local Variables:
;; coding: utf-8-unix
;; fill-column: 80
;; require-final-newline: t
;; End:

;;; minmacs ends here
