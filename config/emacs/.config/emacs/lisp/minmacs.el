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

(defcustom minmacs-core-recipe-sources
  '((org-elpa :local-repo nil)
    (melpa
     :type git
     :host github
     :repo "melpa/melpa"
     :no-build t)
    (gnu-elpa-mirror
     :type git
     :host github
     :repo "emacs-straight/gnu-elpa-mirror"
     :no-build t)
    (emacsmirror-mirror
     :type git
     :host github
     :repo "emacs-straight/emacsmirror-mirror"
     :no-build t))
  "A list of recipe sources for straight to use
Note: recipes, *NOT* packages. The recipes describe where to retrieve them from
and what files to include."
  :group 'minmacs
  :type 'list)

;; Define functions for somewhat declaratively managing packages
;; Satisfy the Emacs compiler
(defvar hgs-emacs-data-directory)
(defvar hgs-emacs-config-directory)
(defvar bootstrap-version)

;; Configure Straight
(defvar straight-base-dir hgs-emacs-data-directory)
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
  `((core . ,(concat hgs-emacs-config-directory "core-lock.el"))
    (custom . ,(concat hgs-emacs-config-directory "custom-lock.el")))
  "Alist of `'(PROFILE-NAME . LOCK-FILE-NAME)'.
Lock file names are either relative to \"straight/versions/\" or absolute
paths.")
(defvar straight-current-profile
  'core
  "Current profile.
Should match an entry in `straight-profiles'. Bind this to different values over
different parts of `core-package.el' in order to have that profile be in
effect.")
(defvar straight-built-in-pseudo-packages
  '(emacs python nadvice seq let-alist)
  "Packages which straight should treat as built-in packages and ignore.")
(defvar straight-recipes-gnu-elpa-user-mirror t
  "If we are using elpa, then make sure to use a Git mirror instead.")

(defvar use-package-compute-statistics t
  "Track how many packages are loaded and their state of initialization.")

;; Minmacs core functions
(defmacro minmacs--as-package-profile (profile &rest body)
  "Specify using straight profile `PROFILE' over `BODY'."
  (declare (indent defun))
  `(let ((straight-current-profile ',profile))
     ,@body))

(defmacro minmacs-as-core-packages (&rest body)
  "Make all package declarations inside `BODY' be specified as core packages."
  (declare (indent defun))
  `(minmacs--as-package-profile core
     ,@body))

(defmacro minmacs-as-custom-packages (&rest body)
  "Make all package declarations inside `BODY' be specified as custom packages."
  (declare (indent defun))
  `(minmacs--as-package-profile custom
     ,@body))

(defun minmacs-get-transitive-dependencies (package)
  "Retrieve a list of all non-built-in dependencies for `PACKAGE'."
  (declare-function straight--get-transitive-dependencies "straight")
  (defvar straight-built-in-pseudo-packages)
  (let ((pkg-name (symbol-name package)))
    (seq-filter #'(lambda (x)
                    (not (member x `(,package
                                     ,@straight-built-in-pseudo-packages))))
                (mapcar #'(lambda (x)
                            (intern x))
                        (straight--get-transitive-dependencies pkg-name)))))

(defun minmacs-bootstrap ()
  "Bootstrap necessary package management libraries."
  (let ((bootstrap-progress
         (make-progress-reporter "Bootstrapping minmacs..." 0 2)))
    (let* ((bootstrap-file (concat straight-base-dir
                                   "straight/repos/straight.el/bootstrap.el"))
           (bootstrap-version 5)
           (repo "raxod502/straight.el")
           (bootstrap-branch "develop")
           (bootstrap-protocol "https"))
      (unless (file-exists-p bootstrap-file)
        (with-current-buffer
            (url-retrieve-synchronously
             (format
              "%s://raw.githubusercontent.com/%s/%s/install.el"
              bootstrap-protocol repo bootstrap-branch)
             'silent 'inhibit-cookies)
          (goto-char (point-max))
          (eval-print-last-sexp)))
      (load bootstrap-file nil 'nomessage)

      (progress-reporter-update bootstrap-progress 1)

      ;; Setup default recipe repositories
      (defvar straight-recipe-repositories)
      (setq straight-recipe-repositories nil)
      (mapc #'straight-use-recipes minmacs-core-recipe-sources)

      ;; Override straight recipe
      (straight-register-package
       `(straight
         :type git
         :host github
         :files ("straight*.el")
         :branch ,straight-repository-branch
         :repo ,repo
         :no-byte-compile t)))

    (minmacs-as-core-packages
      (mapc #'(lambda (pkg)
                (straight-use-package
                 `(,pkg
                   :type git
                   :host github
                   :repo "jwiegley/use-package")))
            '(diminish bind-key use-package))
    (require 'use-package)
    (progress-reporter-done bootstrap-progress))))

(provide 'minmacs)

;; Local Variables:
;; coding: utf-8-unix
;; fill-column: 80
;; require-final-newline: t
;; End:

;;; minmacs ends here
