;;; package-setup.el --- Handles the package setup and language hooks

(require 'cl)
(require 'package)

;; setup package management
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

;; install packages
(defvar-local required-packages
  '(zenburn-theme
    yasnippet
    helm
    magit
    company
    autopair
    rainbow-delimiters
    flycheck
    clang-format))

(when (not package-archive-contents)
  (package-refresh-contents))

(defun packages-installed-p ()
  (loop for p in required-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(defun packages-update ()
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
(load-theme 'zenburn t)

(require 'yasnippet)
(yas-global-mode 1)
(when (not (file-exists-p "~/.emacs.d/snippets"))
  (make-directory "~/.emacs.d/snippets"))
(yas-load-directory "~/.emacs.d/snippets")
(add-hook 'term-mode-hook (lambda()
                            (setq yas-dont-activate t)))


(require 'company)
(company-mode)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0.1)
(add-to-list 'company-backends 'company-elisp)

(setq magit-last-seen-setup-instructions "1.4.0")

(require 'helm-config)

(require 'autopair)
(autopair-global-mode)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(add-hook 'after-init-hook #'global-flycheck-mode)

;; Clang-format C++ buffer on save
;; Hook function
(defun clang-format-before-save ()
  (interactive)
  (when (eq major-mode 'c++-mode) (clang-format-buffer)))

;; Install hook to use clang-format on save
(add-hook 'before-save-hook 'clang-format-before-save)

;; Delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)
