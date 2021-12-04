;;; init.el --- Primary entry-point into config -*- lexical-binding: t; -*-

;;; Commentary:

;; This is where the bulk of the load-order for our configuration, as well as
;; the base configuration directories are defined.

;;; Code:

;; Needed to force emacs to not use stale bytecode
(customize-set-variable 'load-prefer-newer t)

(if (fboundp 'json-serialize)
    (message "Native JSON is available.")
  (message "Native JSON is not available."))

(if (and (fboundp 'native-comp-available)
         (native-comp-available))
    (progn
      (message "Native compilation is available.")
      (when (boundp 'comp-deferred-compilation)
        ;; Enable deferred compilation by default, reasonably early
        (customize-set-variable 'comp-deferred-compilation t)))
  (message "Native compilation is not available."))

;; Stop package.el from starting by default
(customize-set-variable 'package-enable-at-startup nil)
;; Stop it from littering our init
(declare-function package--ensure-init-file "package")
(advice-add #'package--ensure-init-file :override #'ignore)

(require 'cl-lib)
(require 'subr-x)

;; Prevent GC until after init finishes
(customize-set-variable 'gc-cons-threshold most-positive-fixnum)

;; GTK Emacs will react to gconf settings for things like fonts by default. This
;; turns that off.
(define-key special-event-map [config-changed-event] 'ignore)

(defgroup personal nil
  "Core settings for our personal configuration."
  :prefix "hgs-"
  :group 'local
  :version "26.1")

(defconst hgs-has-dynamic-module-support
  (and (functionp 'module-load) (not (null module-file-suffix))))

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
                (concat hgs-user-directory ".local/share")))
           "emacs"))
  "XDG directory specification's data directory."
  :type 'directory
  :group 'personal)

(defcustom hgs-state-directory
  (file-name-as-directory
   (concat (file-name-as-directory
            (or (getenv "XDG_STATE_HOME")
                (concat hgs-user-directory ".local/state")))
           "emacs"))
  "XDG directory specification's state directory."
  :type 'directory
  :group 'personal)

(defcustom hgs-project-directory
  (file-name-as-directory
   (concat (file-name-as-directory hgs-user-directory) "project"))
  "Directory in which our user's projects should be found by default."
  :type 'directory
  :group 'personal)

(defcustom hgs-documents-directory
  (file-name-as-directory
   (or (getenv "XDG_DOCUMENTS_DIR")
       (concat hgs-user-directory "Documents")))
  "Directory in which our user's documents should be placed."
  :type 'directory
  :group 'personal)

(defcustom hgs-org-directory
  (file-name-as-directory
   (concat (file-name-as-directory hgs-documents-directory) "org"))
  "Directory in which our user's org files should be placed."
  :type 'directory
  :group 'personal)

(defcustom hgs-frame-customization-hook '()
  "Hook called when customizing the frame appearance of any Emacs. Takes
the frame being created as an argument."
  :type 'hook
  :group 'personal)

(defcustom hgs-frame-customization-tui-hook '()
  "Hook called when customizing the frame appearance of terminal Emacs. Takes
the frame being created as an argument."
  :type 'hook
  :group 'personal)

(defcustom hgs-frame-customization-gui-hook '()
  "Hook called when customizing the frame appearance of graphical Emacs. Takes
the frame being created as an argument."
  :type 'hook
  :group 'personal)

;; Run frame customization hooks all in one place
(defun hgs--new-frame-setup (&optional frame)
  "Configure the given frame. Should be attached to
`after-make-frame-functions' hook."
  (let ((frame-setup-progress
         (make-progress-reporter "Configuring new frame"))
        (this-frame
         (or frame (selected-frame))))
    (with-selected-frame this-frame
      (run-hook-with-args 'hgs-frame-customization-hook this-frame)

      (if (display-graphic-p)
          (run-hook-with-args 'hgs-frame-customization-gui-hook this-frame)
        (run-hook-with-args 'hgs-frame-customization-tui-hook this-frame)))
    (progress-reporter-done frame-setup-progress)))

(add-hook 'after-make-frame-functions #'hgs--new-frame-setup)

;; Make TLS settings sensible. Emacs is basically a giant insecure lisp REPL. We
;; do it here, because we want this setup before we do *anything* else
;; significant - especially loading packages.
(when (gnutls-available-p)
  (require 'gnutls)
  (customize-set-variable 'gnutls-verify-error t
                          "Always verify on error.")
  (customize-set-variable 'gnutls-min-prime-bits 3072
                          "A sensible number for security.")
  (customize-set-variable
   'gnutls-algorithm-priority
   (concat "SECURE192:+SECURE128:-VERS-ALL"
           (if (and (version< "26.3" emacs-version)
                    (>= libgnutls-version 30605)
                    (not hgs-is-windows))
               ;; Use TLS 1.3 if GnuTLS is both available and high enough
               ;; version
               ":+VERS-TLS1.3"
             ;; Otherwise fallback to 1.2
             ":+VERS-TLS1.2")))
  "Set a relatively secure default priority list of cipher algorithms.")
;; Non-GnuTLS path - Note that `tls' is deprecated, so this may no longer work
;; in future versions
(when (require 'tls nil 'noerror)
  (customize-set-variable 'tls-checktrust t
                          "We should accept un-trusted certificates.")
  (customize-set-variable
   'tls-program
   '(;; Emacs will use gnutls by default if built with it, but we prefer openssl
     ;; when it isn't.
     "openssl s_client -connect %h:%p -CAfile %t -nbio -no_ssl3 -no_tls1 \
-no_tls1_1 -ign_eof"
     ;; Use gnutls if we can't use openssl
     "gnutls-cli -p %p --dh-bits=3072 --ocsp -x509cafile=%t --strict-tofu \
--priority='SECURE192:+SECURE128:-VERS-ALL:+VERS-TLS1.2:+VERS-TLS1.3' %h"
     ;; If all else fails...
     "gnutls-cli -p %p %h")
   "Sensible TLS program invocations (in-order of priority) when GnuTLS isn't
enabled."))

;; Chnage the built-in init directories for old emacs versions using ~/.emacs
(setq user-init-file load-file-name)
(setq user-emacs-directory hgs-config-directory)
;; Stop emacs from littering our $HOME
(if (< emacs-major-version 27)
    (let ((dir (concat hgs-user-directory ".emacs.d")))
      (when (file-accessible-directory-p dir)
        (delete-directory dir 'recursive))))

(let ((default-directory (concat (file-name-directory load-file-name)
                                 "lisp")))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(defmacro load-if-exists (file-path &optional nomessage nosuffix must-suffix)
  "Load file at `FILE-PATH' if it exists. Mainly just for clarity

This passes through the passed `NOSUFFIX' and `MUST-SUFFIX' to the underlying
load call as one might expect."
  `(load ,file-path 'noerror ,nomessage ,nosuffix ,must-suffix))

;; Load the core package management primitives we use
(load (concat hgs-config-directory "lisp/minmacs")
      nil 'nomessage)
(minmacs-bootstrap)

;; Declaratively specify packages
(load (concat  hgs-config-directory "core-package")
      nil 'nomessage)
(let (;; (straight-recipe-repositories nil)
      ;; (straight-recipe-overrides nil)
      (straight-current-profile 'custom))
  (load-if-exists (concat hgs-config-directory "custom-package")))

;; Load core configuration modules
(load (concat hgs-config-directory "core-config")
      nil 'nomessage)
(load-if-exists (concat hgs-config-directory "custom-config"))

;; Setup customization paths
;; We want the customize interface to alter a local overrides file
(customize-set-variable 'custom-file
                        (concat hgs-config-directory "custom-customization.el"))
(load-if-exists custom-file)

;; Same as above for abbreviations
(customize-set-variable 'abbrev-file-name
                        (concat hgs-config-directory "custom-abbreviation.el"))
(load-if-exists abbrev-file-name)

;; For non daemon run we want to manually run frame setup hooks that have been
;; configured, as init file runs after the initial frame is created.
(unless (daemonp)
  (hgs--new-frame-setup))

;; Reset GC threshold to 100MB
(customize-set-variable 'gc-cons-threshold (* 1024 1024 100))

;; Local Variables:
;; coding: utf-8-unix
;; fill-column: 80
;; require-final-newline: t
;; End:

;;; init.el ends here
