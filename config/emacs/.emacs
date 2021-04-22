;;; .emacs --- Delegates configuration to XDG -*- lexical-binding: t; -*-

;;; Commentary:

;; Newer versions of emacs (>=27) allow us to load directly from XDG paths, but
;; we use earlier versions, so we provide this file to delegate configuration
;; into the appropriate subdirectory. This file should do nothing else.
;;

;;; Code:

;; Simply defer actual config to XDG (newer versions of emacs don't need this)
(let* ((user-home
         (or (getenv "HOME") (getenv "USERPROFILE")))
       (xdg-config-directory
         (or (getenv "XDG_CONFIG_HOME")
             (concat (file-name-as-directory user-home) ".config")))
       (emacs-config-directory
         (file-name-as-directory
           (concat (file-name-as-directory xdg-config-directory) "emacs"))))
  (message "Loading actual configuration from \"%s\"..." emacs-config-directory)
  (load (concat emacs-config-directory "init") nil 'nomessage))

;;; .emacs ends here
