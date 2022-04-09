;;; init.el --- Primary entry-point into config -*- lexical-binding: t; -*-

;;; Commentary:

;; This is where the bulk of the load-order for our configuration, as well as
;; the base configuration directories are defined.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(let ((default-directory (concat (file-name-directory load-file-name)
                                 "lisp")))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))
(require 'hgs-core)
(require 'hgs-file)

(unless (featurep 'hgs-pre-init)
  (require 'hgs-pre-init)
  (hgs-run-pre-init))

(hgs-enable-new-frame-setup)
(hgs-fix-tls)
(hgs-fix-init-paths)

;; Load the core package management primitives we use
(require 'minmacs)
(minmacs-bootstrap)

;; Declaratively specify packages
(require 'core-package (concat hgs-emacs-config-directory "core-package"))
(let (;; (straight-recipe-repositories nil)
      ;; (straight-recipe-overrides nil)
      (straight-current-profile 'custom))
  (load-if-exists (concat hgs-emacs-config-directory "custom-package")))

;; Load core configuration modules
(require 'core-config (concat hgs-emacs-config-directory "core-config"))
(load-if-exists (concat hgs-emacs-config-directory "custom-config"))

;; Setup customization paths
;; We want the customize interface to alter a local overrides file
(customize-set-variable 'custom-file
                        (concat hgs-emacs-config-directory
                                "custom-customization.el"))
(load-if-exists custom-file)

;; Same as above for abbreviations
(customize-set-variable 'abbrev-file-name
                        (concat hgs-emacs-config-directory
                                "custom-abbreviation.el"))
(load-if-exists abbrev-file-name)

(require 'hgs-post-init)
(hgs-run-post-init)

;; Local Variables:
;; coding: utf-8-unix
;; fill-column: 80
;; require-final-newline: t
;; End:

;;; init.el ends here
