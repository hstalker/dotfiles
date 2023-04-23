;;; hgs-core.el --- Core Emacs primitives -*- lexical-binding: t; -*-

;;; Commentary:

;; Provides fundamentals to be used across the configuration

;;; Code:

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
   (or (getenv "XDG_CONFIG_HOME")
       (concat hgs-user-directory ".config")))
  "XDG directory specification's config directory."
  :type 'directory
  :group 'personal)

(defcustom hgs-emacs-config-directory
  (file-name-as-directory (concat hgs-config-directory "emacs"))
  "Emacs config directory."
  :type 'directory
  :group 'personal)

(defcustom hgs-cache-directory
  (file-name-as-directory
   (or (getenv "XDG_CACHE_HOME")
       (concat hgs-user-directory ".cache")))
  "XDG directory specification's cache directory."
  :type 'directory
  :group 'personal)

(defcustom hgs-emacs-cache-directory
  (file-name-as-directory (concat hgs-cache-directory "emacs"))
  "Emacs cache directory."
  :type 'directory
  :group 'personal)

(defcustom hgs-data-directory
  (file-name-as-directory
   (or (getenv "XDG_DATA_HOME")
       (concat hgs-user-directory ".local/share")))
  "XDG directory specification's data directory."
  :type 'directory
  :group 'personal)

(defcustom hgs-emacs-data-directory
  (file-name-as-directory (concat hgs-data-directory "emacs"))
  "Emacs data directory."
  :type 'directory
  :group 'personal)

(defcustom hgs-state-directory
  (file-name-as-directory
   (or (getenv "XDG_STATE_HOME")
       (concat hgs-user-directory ".local/state")))
  "XDG directory specification's state directory."
  :type 'directory
  :group 'personal)

(defcustom hgs-emacs-state-directory
  (file-name-as-directory (concat hgs-state-directory "emacs"))
  "Emacs state directory."
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


(defun hgs-fix-tls ()
  "Make TLS settings sensible.

Emacs is basically a giant insecure lisp REPL. We do it here,
because we want this setup before we do *anything* else
significant - especially loading packages."
  (when (gnutls-available-p)
    (require 'gnutls)
    (customize-set-variable 'gnutls-verify-error t
                            "Always verify on error.")
    (customize-set-variable 'gnutls-min-prime-bits 3072
                            "A sensible number for security.")
    (customize-set-variable
     'gnutls-algorithm-priority
     (let ((supports-tls1.3 (and (version< "26.3" emacs-version)
                                 (>= libgnutls-version 30605)
                                 (not hgs-is-windows))))
       (concat "SECURE192:+SECURE128:-VERS-ALL"
               ;; Use TLS 1.3 if GnuTLS is both available & high enough version
               (if supports-tls1.3 ":+VERS-TLS1.3" nil)
               ":+VERS-TLS1.2"))
    "Set a relatively secure default priority list of cipher algorithms."))
  ;; Non-GnuTLS path - Note that `tls' is deprecated, so this may no longer work
  ;; in future versions
  (when (require 'tls nil 'noerror)
    (customize-set-variable 'tls-checktrust t
                            "We should accept un-trusted certificates.")
    (customize-set-variable
     'tls-program
     '(;; Emacs will use gnutls by default if built with it, but we prefer
       ;; openssl when it isn't.
       "openssl s_client -connect %h:%p -CAfile %t -nbio -no_ssl3 -no_tls1 \
-no_tls1_1 -ign_eof"
       ;; Use gnutls if we can't use openssl
       "gnutls-cli -p %p --dh-bits=3072 --ocsp -x509cafile=%t --strict-tofu \
--priority='SECURE192:+SECURE128:-VERS-ALL:+VERS-TLS1.3:+VERS-TLS1.2' %h"
       ;; If all else fails...
       "gnutls-cli -p %p %h")
     "Sensible TLS program invocations (in-order of priority) when GnuTLS isn't
enabled.")))

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
          (progn
            (message "Detected graphics display")
            (run-hook-with-args 'hgs-frame-customization-gui-hook this-frame))
        (progn
          (message "Detected terminal display")
          (run-hook-with-args 'hgs-frame-customization-tui-hook this-frame))))
    (progress-reporter-done frame-setup-progress)))

(defun hgs-enable-new-frame-setup ()
  ;; Setting up a function that runs for all new frames in all contexts (useful
  ;; or custom styling) is an utter mess in Emacs, and is both poorly
  ;; understood and documented.
  ;; N.B. There is still a minor bug here that
  ;; causes the hook to be run twice upon initial frame
  (if (daemonp)
      (progn
        ;; Needed for configuring 2nd frame onwards
        (add-hook 'after-make-frame-functions #'hgs--new-frame-setup)
        (when (version<= "27.0" emacs-version)
          ;; Needed for configuring initial client frame
          (add-hook 'server-after-make-frame-hook #'hgs--new-frame-setup)))
    (progn
      ;; Needed for configuring 2nd frame onwards
      (add-hook 'after-make-frame-functions #'hgs--new-frame-setup)
      ;; Needed for configuring initial frame
      (add-hook 'window-setup-hook #'hgs--new-frame-setup))))

(defun hgs-fix-init-paths ()
  ;; Change the built-in init directories for old emacs versions using ~/.emacs
  (setq user-init-file load-file-name)
  (setq user-emacs-directory hgs-emacs-config-directory)
  ;; Stop emacs from littering our $HOME
  (if (< emacs-major-version 27)
      (let ((dir (concat hgs-user-directory ".emacs.d")))
        (when (file-accessible-directory-p dir)
          (delete-directory dir 'recursive)))))

(provide 'hgs-core)

;; Local Variables:
;; mode: emacs-lisp
;; coding: utf-8-unix
;; fill-column: 80
;; require-final-newline: t
;; End:

;;; hgs-core.el ends here
