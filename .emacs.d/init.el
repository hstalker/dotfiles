(require 'cl)
(require 'package)

;; setup package management
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("melpa" . "http://melpa.org/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

;; install packages
(setq required-packages
      '(zenburn-theme
	yasnippet
	company
	company-cabal
	company-irony
	company-anaconda
	autopair
	rainbow-delimiters
	flycheck))

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
(yas-load-directory "~/.emacs.d/snippets")
(add-hook 'term-mode-hook (lambda()
			    (setq yas-dont-activate t)))

(company-mode)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0.1)
(add-to-list 'company-backends 'company-cabal)
(add-to-list 'company-backends 'company-elisp)
(add-to-list 'company-backends 'company-anaconda)
(add-to-list 'company-backends 'company-irony)
(add-hook 'irony-mode-hook 'company-irony-setup-being-commands)

(require 'autopair)
(autopair-global-mode)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(add-hook 'after-init-hook #'global-flycheck-mode)

;; save files on loss of focus
(add-hook 'focus-out-hook (lambda()
			    ((interactive)
			     (save-some-buffers t))))

;; get rid of GUI menu stuff
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; remove startup screen
(setq inhibit-startup-screen t)
(setq auto-save-file-name-transforms `((".*" , temporary-file-directory t))
      backup-directory-alist `((".*" . , temporary-file-directory)))

;; enable the clipboard
(setq x-select-enable-clipboard t)

;; highlight current line
(global-hl-line-mode 1)

;; set standard indent size
(setq standard-indent 4)

;; disable backups
(setq make-backup-files nil)

;; enable line and column numbering
(line-number-mode t)
(global-linum-mode t)
(column-number-mode t)

;; set how emacs justifies paragraphs
(setq-default fill-column 80)

;; set emacs to automatically wrap and insert newlines upon wrapping
(setq auto-fill-mode 1)

;; default to text-mode
(setq default-major-mode 'text-mode)

;; set default font
(set-frame-font "DejaVu Sans Mono-11" nil t)
