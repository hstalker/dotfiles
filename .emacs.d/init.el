;;; init.el --- Personal elisp config for Emacs
;;; Commentary:
;; At the moment all Emacs configurations are placed here.
;; Uses standard packaging archives and mechanisms.

;;; Code:
(require 'cl-lib)
(require 'package)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package handling and setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup package management web repo paths
(setq package-enable-at-startup nil)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; install use-package
(defun bootstrap-use-package ()
  "Update all installed packages if possible."
  (interactive)
  (unless (package-installed-p 'use-package)
    ;; check for new package versions
    (message "Refreshing package database...")
    (package-refresh-contents)
    (message "Bootstrapping use-package...")
    (package-install 'use-package)))

;; do the initial bootstrap if the packages directory can't be found
(unless (file-directory-p (concat user-emacs-directory "elpa"))
  (message "Package directory 'elpa' not found...")
  (bootstrap-use-package))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load packages, themes and plugins
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helps the following packaging configurations be easier to manage
(require 'use-package)


(use-package solarized-theme
  :ensure t
  :init
  ;; make the fringe stand out from the background
  (defvar solarized-distinct-fringe-background t)
  ;; Don't change the font for some headings and titles
  (defvar solarized-use-variable-pitch nil)
  ;; make the modeline high contrast
  (defvar solarized-high-contrast-mode-line t)
  ;; Use less bolding
  (defvar solarized-use-less-bold t)
  ;; Use more italics
  (defvar solarized-use-more-italic t)
  ;; Use less colors for indicators such as git:gutter, flycheck and similar
  (defvar solarized-emphasize-indicators nil)
  ;; Don't change size of org-mode headlines (but keep other size-changes)
  (defvar solarized-scale-org-headlines nil)
  ;; Avoid all font-size changes
  (defvar solarized-height-minus-1 1.0)
  (defvar solarized-height-plus-1 1.0)
  (defvar solarized-height-plus-2 1.0)
  (defvar solarized-height-plus-3 1.0)
  (defvar solarized-height-plus-4 1.0)
  :config
  (load-theme 'solarized-dark t))


(use-package ivy
  :ensure t
  :diminish ivy-mode
  :init
  (defvar ivy-use-virtual-buffers t)
  (defvar enable-recursive-minibuffers t)
  :config
  (ivy-mode))


(use-package yasnippet
  :ensure t
  :diminish yas-mode
  :config
  (yas-global-mode 1)
  (when (not (file-exists-p "~/.emacs.d/snippets"))
    (make-directory "~/.emacs.d/snippets"))
  (yas-load-directory "~/.emacs.d/snippets")
  (add-hook 'term-mode-hook (lambda()
                              (setq yas-dont-activate-functions t))))


(use-package magit
  :ensure t)


(use-package autopair
  :ensure t
  :diminish autopair-mode
  :config
  (autopair-global-mode))


(use-package rainbow-delimiters
  :ensure t
  :diminish rainbow-delimiters-mode
  :config
  (dolist (hook '(prog-mode-hook comint-mode-hook))
    (add-hook hook 'rainbow-delimiters-mode)))


(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))


(use-package clang-format
  :ensure t
  :config
  (defun clang-format-before-save ()
    "Clang-format C++ buffer on save hook function."
    (interactive)
    (when (eq major-mode 'c++-mode) (clang-format-buffer)))

  ;; install hook to use clang-format on save
  (add-hook 'before-save-hook 'clang-format-before-save))


(use-package powerline
  :ensure t
  :config
  (powerline-center-evil-theme))


(use-package cygwin-mount
  :ensure t
  :if (memq system-type '(windows-nt cygwin))
  :config
  (cygwin-mount-activate))


(use-package ace-window
  :ensure t
  :init
  (defvar aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))


(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode)


(use-package evil-leader
  :ensure t
  :diminish evil-leader-mode
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")

  (evil-leader/set-key
    "e" 'find-file
    "b" 'switch-to-buffer
    "init" (lambda () (interactive) (find-file user-init-file))
    "w" 'ace-window))


(use-package evil
  :ensure t
  :diminish evil-mode
  :config
  (evil-mode))


(use-package evil-escape
  :ensure t
  :diminish evil-escape-mode
  :config
  (evil-escape-mode)
  (setq-default evil-escape-key-sequence "kj")
  (setq-default evil-escape-delay 0.1))


(use-package evil-magit
  :ensure t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; non-package related built-in settings changes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get rid of GUI menu stuff
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; remove startup screen
(setq-default inhibit-startup-screen t)
(setq-default auto-save-file-name-transforms `((".*" , temporary-file-directory t))
              backup-directory-alist `((".*" . , temporary-file-directory)))

;; enable the clipboard
(setq-default x-select-enable-clipboard t)

;; highlight current line
(global-hl-line-mode 1)

;; enable line and column numbering
(global-linum-mode)

;; change cursor to line
(setq-default cursor-type 'bar)

;; set standard indent size
(setq-default standard-indent 4)
(setq-default tab-width 4)
;; use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; disable backups
(setq-default make-backup-files nil)

;; set how emacs justifies paragraphs
(setq-default fill-column 80)

;; set how Emacs displays code that goes over 80 columns
(setq-default whitespace-line-column 80
              whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)


;; set emacs to automatically wrap and insert newlines upon wrapping
(auto-fill-mode)

;; set default font
(set-frame-font "DejaVu Sans Mono-11" nil t)
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-11"))
(add-to-list 'initial-frame-alist '(font . "DejaVu Sans Mono-11"))

;; start emacs maximised
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; remove alarm bell
(setq ring-bell-function 'ignore)

;; delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; change yes or no prompts to be y or n prompts
(defalias 'yes-or-no-p #'y-or-n-p)

;; utils for byte-compiling the .emacs.d directory automatically
(defun byte-compile-init-dir ()
  "Byte-compile your Emacs elisp init code."
  (interactive)
  (byte-recompile-directory user-emacs-directory 0))

(defun remove-elc-on-save ()
  "Delete .elc files on save to .el files, as the .elc need recompiling."
  (add-hook 'after-save-hook
            (lambda ()
              (let ((bcf (concat (file-name-sans-extension buffer-file-name) ".elc")))
                (if (file-exists-p bcf)
                    (delete-file bcf))))
            nil
            t))

(add-hook 'emacs-lisp-mode-hook 'remove-elc-on-save)

;; utils for cleaning up whole buffers
(defun untabify-buffer ()
  "Change tabs to spaces on whole buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  "Does Emacs' standard indenting on the whole buffer."
  (interactive)
  (indent-region (point-min) (point-max)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; automatically manipulated elisp -- DON'T TOUCH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (evil-magit evil-escape evil-leader ace-window cygwin-mount powerline clang-format flycheck rainbow-delimiters autopair magit yasnippet use-package solarized-theme ivy))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
