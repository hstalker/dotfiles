;;; settings.el --- The general settings for emacs

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

;; change cursor to line
(setq-default cursor-type 'bar)

;; set standard indent size
(setq-default standard-indent 4)
(setq-default tab-width 4)
;; use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; disable backups
(setq-default make-backup-files nil)

;; enable line and column numbering
(line-number-mode t)
(global-linum-mode t)
(column-number-mode t)

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
(c-add-style "hgs"
             '("k&r"
               (c-offsets-alist
                (innamespace . -))
               (indent-tabs-mode . nil)
               (c-basic-offset . 4)))
(setq c-default-style "hgs")
