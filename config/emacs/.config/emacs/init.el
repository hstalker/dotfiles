;;; init.el --- Primary entry-point into config -*- lexical-binding: t; -*-

;;; Commentary:

;; This is where the bulk of the load-order for our configuration, as well as
;; the base configuration directories are defined.

;;; Code:

;; HACK: Work around a bug in GUI emacs 26 causing rendering issues
;; Bug report: https://lists.gnu.org/r/bug-gnu-emacs/2018-04/msg00658.html
(when (eq emacs-major-version 26)
  (push '(inhibit-double-buffering . t) default-frame-alist))

;; Needed to force emacs to not use stale bytecode
(setq load-prefer-newer t)

;; Stop package.el from starting by default
(customize-set-variable 'package-enable-at-startup nil)
;; Stop is from littering our init
(advice-add #'package--ensure-init-file :override #'ignore)

(require 'cl-lib)
(require 'subr-x)

;; Prevent GC until after init finishes
(customize-set-variable 'gc-cons-threshold most-positive-fixnum)

(defgroup personal nil
  "Core settings for our personal configuration."
  :prefix "hgs-"
  :group 'local
  :version "26.1")

(defconst hgs-is-mac (eq system-type 'darwin))
(defconst hgs-is-linux (eq system-type 'gnu/linux))
(defconst hgs-is-bsd
  (or hgs-is-mac
      (eq system-type 'berkeley-unix)))
(defconst hgs-is-windows
  (memq system-type
        '(cygwin windows-nt ms-dos)))

(defcustom hgs-user-directory
  (file-name-as-directory (if hgs-is-windows
                              (getenv "USERPROFILE")
                            (getenv "HOME")))
  "User's home directory (absolute path)."
  :type 'directory
  :group 'personal)

(defcustom hgs-config-directory
  (file-name-as-directory
    (concat (file-name-as-directory
              (or (getenv "XDG_CONFIG_HOME")
                  (concat hgs-user-directory ".config")))
            "emacs"))
  "XDG directory specification's config directory."
  :type 'directory
  :group 'personal)

(defcustom hgs-cache-directory
  (file-name-as-directory
    (concat (file-name-as-directory
              (or (getenv "XDG_CACHE_HOME")
                  (concat hgs-user-directory ".cache")))
            "emacs"))
  "XDG directory specification's cache directory."
  :type 'directory
  :group 'personal)

(defcustom hgs-data-directory
  (file-name-as-directory
    (concat (file-name-as-directory
              (or (getenv "XDG_DATA_HOME")
                  (concat hgs-user-directory ".local" "share")))
            "emacs"))
  "XDG directory specification's data directory."
  :type 'directory
  :group 'personal)

(defcustom hgs-project-directory
  (concat (file-name-as-directory hgs-user-directory) "project")
  "Directory in which our user's projects should be found by default."
  :type 'directory
  :group 'personal)

(defcustom hgs-org-directory
  (file-name-as-directory
    (concat (file-name-as-directory hgs-user-directory) "org"))
  "Directory in which our user's org files should be placed."
  :type 'directory
  :group 'personal)

;; Chnage the built-in init directories for old emacs versions using ~/.emacs
(setq user-init-file load-file-name)
(setq user-emacs-directory (file-name-as-directory hgs-config-directory))
;; Stop emacs from littering our $HOME
(if (< emacs-major-version 27)
    (let ((dir (concat (file-name-as-directory hgs-user-directory) ".emacs.d")))
      (when (file-accessible-directory-p dir)
        (delete-directory dir 'recursive))))

(let ((default-directory (concat (file-name-directory load-file-name)
                                 "lisp")))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(defun load-if-exists (file-path &optional noerror nomessage nosuffix
                                 must-suffix)
  "Load file at `FILE-PATH' if it exists.

This passes through the passed `NOERROR', `NOMESSAGE', `NOSUFFIX' and
`MUST-SUFFIX' to the underlying load call as one might expect."
  (when (file-exists-p file-path)
    (load file-path noerror nomessage nosuffix must-suffix)))

;; Load the core package management primitives we use
(load (concat (file-name-as-directory hgs-config-directory) "lisp/minmacs")
      nil 'nomessage)
(minmacs-bootstrap)

;; Declaratively specify packages
(load (concat (file-name-as-directory hgs-config-directory) "core-package")
      nil 'nomessage)
(load-if-exists (concat (file-name-as-directory hgs-config-directory)
                        "custom-package"))

;; Load core configuration modules
(load (concat (file-name-as-directory hgs-config-directory) "core-config")
      nil 'nomessage)
(load-if-exists (concat (file-name-as-directory hgs-config-directory)
                        "custom-config"))

;; Setup customization paths
;; We want the customize interface to alter a local overrides file
(customize-set-variable 'custom-file
                        (concat (file-name-as-directory hgs-config-directory)
                                "custom-customization.el"))
(load-if-exists custom-file)

;; Same as above for abbreviations
(customize-set-variable 'abbrev-file-name
                        (concat (file-name-as-directory hgs-config-directory)
                                "custom-abbreviation.el"))
(load-if-exists abbrev-file-name)

;; Reset GC threshold to 100MB
(customize-set-variable 'gc-cons-threshold (* 1024 1024 100))

;; Local Variables:
;; coding: utf-8-unix
;; fill-column: 80
;; require-final-newline: t
;; End:

;;; init.el ends here
