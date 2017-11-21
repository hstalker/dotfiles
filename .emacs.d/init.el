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
;; must come before configurations of packages
(package-initialize)
;; setup package management
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("marmalade" . "https://marmalade-repo.org/packages/")))
(package-initialize)

;; install packages
(defvar-local required-packages
  '(solarized-theme
    ivy
    yasnippet
    magit
    autopair
    rainbow-delimiters
    flycheck
    clang-format
    powerline
    cygwin-mount
    ace-window
    evil-leader
    evil
    evil-escape
    evil-magit))

(when (not package-archive-contents)
  (package-refresh-contents))

(defun packages-installed-p ()
  "Predicate checking whether all required packages are already installed."
  (cl-loop for p in required-packages
        when (not (package-installed-p p)) return nil
        finally return t
        ))

(defun packages-update ()
  "Update all installed packages if possible."
  (interactive)
  (unless (packages-installed-p)
    ;; check for new package versions
    (message "%s" "Refreshing package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install updates
    (dolist (p required-packages)
      (when (not (package-installed-p p))
        (package-install p)))))

(unless (file-exists-p "elpa")
  (packages-update))


;; load packages, themes and plugins

;; SOLARIZED
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
(load-theme 'solarized-dark t)


;; IVY
(ivy-mode)
(defvar ivy-use-virtual-buffers t)
(defvar enable-recursive-minibuffers t)


;; YASNIPPET
(require 'yasnippet)
(yas-global-mode 1)
(when (not (file-exists-p "~/.emacs.d/snippets"))
  (make-directory "~/.emacs.d/snippets"))
(yas-load-directory "~/.emacs.d/snippets")
(add-hook 'term-mode-hook (lambda()
                            (setq yas-dont-activate-functions t)))


;; MAGIT
(require 'magit)


;; AUTOPAIR
(require 'autopair)
(autopair-global-mode)


;; RAINBOW DELIMITERS
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)


;; FLYCHECK
(add-hook 'after-init-hook #'global-flycheck-mode)


;; CLANG-FORMAT
(defun clang-format-before-save ()
  "Clang-format C++ buffer on save hook function."
  (interactive)
  (when (eq major-mode 'c++-mode) (clang-format-buffer)))

;; install hook to use clang-format on save
(add-hook 'before-save-hook 'clang-format-before-save)


;; POWERLINE
(require 'powerline)
(powerline-center-evil-theme)


;; CYGWIN-MOUNT
(when (or (eq system-type 'windows-nt) (eq system-type 'cygwin))
  (require 'cygwin-mount)
  (cygwin-mount-activate)
  )


;; ACE-WINDOW
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(require 'ace-window)


;; EVIL-LEADER
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")

(defun find-user-init-file ()
  "Instantly edit the `user-init-file' in current window."
  (interactive)
  (find-file user-init-file))

(evil-leader/set-key
  "e" 'find-file
  "b" 'switch-to-buffer
  "k" 'kill-buffer
  "in" 'find-user-init-file
  "w" 'ace-window)


;; EVIL
(require 'evil)
(evil-mode)


;; EVIL-ESCAPE
(evil-escape-mode)
(setq-default evil-escape-key-sequence "kj")
(setq-default evil-escape-delay 0.1)


;; EVIL-MAGIT
(require 'evil-magit)


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

;; enable relative line and column numbering
(linum-mode)

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

;; set emacs to automatically wrap and insert newlines upon wrapping
(auto-fill-mode)

;; set default font
(set-frame-font "DejaVu Sans Mono-11" nil t)

;; start emacs maximised
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; remove alarm bell
(setq ring-bell-function 'ignore)

;; set C/C++ style
(c-add-style "personal"
             '("k&r")
             )

;; Delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (cygwin-mount evil-leader yasnippet solarized-theme rainbow-delimiters magit linum-relative helm goto-last-change flycheck evil clang-format autopair))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
