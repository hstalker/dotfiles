;;; core-package.el --- Core package specification -*- lexical-binding: t; -*-

;;; Commentary:

;; Provides the baseline API and tools to declaratively specify package loading

;;; Code:

;; Specify packages


(straight-use-package 'org-bullets)
(straight-use-package 'clang-format)
(straight-use-package 'clang-format+)
(straight-use-package 'exec-path-from-shell)
(straight-use-package 'hydra)
(straight-use-package 'projectile)
(straight-use-package 'helm)
(straight-use-package 'helm-projectile)
(straight-use-package 'helm-descbinds)
(straight-use-package 'helm-swoop)
(straight-use-package 'solarized-theme)
(straight-use-package 'async)
(straight-use-package 'magit)
(straight-use-package 'ws-butler)
(straight-use-package 'expand-region)
(straight-use-package 'smartparens)
(straight-use-package 'yasnippet)
(straight-use-package 'company)
(straight-use-package 'undo-tree)
(straight-use-package 'flycheck)
(straight-use-package 'flyspell)
(straight-use-package 'which-key)
(straight-use-package 'transpose-frame)
(straight-use-package 'rainbow-delimiters)
(straight-use-package 'cmake-mode)
(straight-use-package 'yaml-mode)
(straight-use-package 'csv-mode)
(straight-use-package 'lua-mode)
(straight-use-package 'markdown-mode)

;; Local Variables:
;; coding: utf-8-unix
;; fill-column: 80
;; require-final-newline: t
;; End:

;;; core-package.el ends here
