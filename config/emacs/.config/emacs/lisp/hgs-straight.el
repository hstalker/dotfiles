;;; hgs-straight.el --- Bootstrap Straight -*- lexical-binding: t; -*-

;;; Commentary:

;; Provides utilities & fundamentals for Straight-based package management

;;; Code:

(require 'hgs-core)

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
  `((core . ,(concat hgs-emacs-config-directory "lock.el")))
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
(defvar straight-recipes-gnu-elpa-use-mirror t
  "If we are using elpa, then make sure to use a Git mirror instead.")
(defvar straight-recipes-emacsmirror-use-mirror t
  "If we are using emacsmirror, then make sure to use a Git mirror instead.")

(defvar use-package-compute-statistics t
  "Track how many packages are loaded and their state of initialization.")

(defun hgs-straight-bootstrap ()
  "Bootstrap necessary package management libraries."
  (let ((bootstrap-progress
         (make-progress-reporter "Bootstrapping straight..." 0 2)))
    (let* ((bootstrap-file (concat straight-base-dir
                                   "straight/repos/straight.el/bootstrap.el"))
           (bootstrap-version 5)
           (repo "radian-software/straight.el")
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

      ;; Override straight recipe
      (straight-register-package
       `(straight
         :type git
         :host github
         :files ("straight*.el")
         :branch ,straight-repository-branch
         :repo ,repo
         :no-byte-compile t)))

    ;; GNU Savannah (the default) gets DDoSed near constantly, so we prefer a Github mirror
    (custom-set-variables
     '(straight-recipe-overrides '((nil
                                    (nongnu-elpa :type git
                                                 :repo "https://github.com/emacsmirror/nongnu_elpa"
                                                 :depth (full single-branch)
                                                 :local-repo "nongnu-elpa"
                                                 :build nil)))))

    (let ((straight-current-profile 'core))
      (mapc #'(lambda (pkg)
                (straight-use-package
                 `(,pkg
                   :type git
                   :host github
                   :repo "jwiegley/use-package")))
            '(diminish bind-key use-package))
      (require 'use-package)
      (progress-reporter-done bootstrap-progress))))

(provide 'hgs-straight)

;; Local Variables:
;; mode: emacs-lisp
;; coding: utf-8-unix
;; fill-column: 80
;; require-final-newline: t
;; End:

;;; hgs-straight.el ends here
