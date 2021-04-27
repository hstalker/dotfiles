;;; core-package.el --- Core package specification -*- lexical-binding: t; -*-

;;; Commentary:

;; Provides the baseline API and tools to declaratively specify package loading

;;; Code:

;; Specify packages

(declare-function straight-use-package "straight")

(straight-use-package
 '(dash
   :type git
   :host github
   :repo "magnars/dash.el"))
(straight-use-package
 '(s
   :type git
   :host github
   :repo "magnars/s.el"))
(straight-use-package
 '(f
   :type git
   :host github
   :repo "rejeep/f.el"))
(straight-use-package
 '(org-bullets
   :type git
   :host github
   :repo "sabof/org-bullets"))
;; clang-format.el is generally hosted upstream in LLVM, with a wrapper MELPA
;; package.
(straight-use-package
 '(clang-format
   :type git
   :host github
   :repo "emacsmirror/clang-format"))
(straight-use-package
 '(clang-format+
   :type git
   :host github
   :repo "SavchenkoValeriy/emacs-clang-format-plus"))
(straight-use-package
 '(exec-path-from-shell
   :type git
   :host github
   :repo "purcell/exec-path-from-shell"))
(straight-use-package
 '(hydra
   :type git
   :host github
   :repo "abo-abo/hydra"))
(straight-use-package
 '(projectile
   :type git
   :host github
   :repo "bbatsov/projectile"))
(straight-use-package
 '(selectrum
   :type git
   :host github
   :repo "raxod502/selectrum"))
(straight-use-package
 '(prescient
   :type git
   :host github
   :repo "raxod502/prescient.el"))
(straight-use-package
 '(selectrum-prescient
   :type git
   :host github
   :repo "raxod502/prescient.el"))
(straight-use-package
 '(company-prescient
   :type git
   :host github
   :repo "raxod502/prescient.el"))
(straight-use-package
 '(consult
   :type git
   :host github
   :repo "minad/consult"))
(straight-use-package
 '(consult-flycheck
   :type git
   :host github
   :repo "minad/consult"))
(straight-use-package
 '(marginalia
   :type git
   :host github
   :repo "minad/marginalia"))
(straight-use-package
 '(embark
   :type git
   :host github
   :repo "oantolin/embark"))
(straight-use-package
 '(embark-consult
   :type git
   :host github
   :repo "oantolin/embark"))
(straight-use-package
 '(flyspell-correct
   :type git
   :host github
   :repo "d12frosted/flyspell-correct"))
(straight-use-package
 '(solarized-theme
   :type git
   :host github
   :repo "bbatsov/solarized-emacs"))
(straight-use-package
 '(async
   :type git
   :host github
   :repo "jwiegley/emacs-async"))
(straight-use-package
 '(with-editor
   :type git
   :host github
   :repo "magit/with-editor"))
(straight-use-package
 '(transient
   :type git
   :host github
   :repo "magit/transient"))
(straight-use-package
 '(magit
   :type git
   :host github
   :repo "magit/magit"))
(straight-use-package
 '(ws-butler
   :type git
   :host github
   :repo "lewang/ws-butler"))
(straight-use-package
 '(expand-region
   :type git
   :host github
   :repo "magnars/expand-region.el"))
(straight-use-package
 '(smartparens
   :type git
   :host github
   :repo "Fuco1/smartparens"))
(straight-use-package
 '(yasnippet
   :type git
   :host github
   :repo "joaotavora/yasnippet"))
(straight-use-package
 '(yasnippet-snippets
   :type git
   :host github
   :repo "AndreaCrotti/yasnippet-snippets"))
(straight-use-package
 '(company
   :type git
   :host github
   :repo "company-mode/company-mode"))
(straight-use-package
 '(undo-tree
   :type git
   :host gitlab
   :repo "tsc25/undo-tree"))
(straight-use-package
 '(flycheck
   :type git
   :host github
   :repo "flycheck/flycheck"))
(straight-use-package
 '(which-key
   :type git
   :host github
   :repo "justbur/emacs-which-key"))
(straight-use-package
 '(transpose-frame
   :type git
   :host github
   :repo "emacsorphanage/transpose-frame"))
(straight-use-package
 '(rainbow-delimiters
   :type git
   :host github
   :repo "Fanael/rainbow-delimiters"))
(straight-use-package
 '(cmake-mode
   :type git
   :host github
   :repo "emacsmirror/cmake-mode"))
(straight-use-package
 '(yaml-mode
   :type git
   :host github
   :repo "yoshiki/yaml-mode"))
(straight-use-package
 '(csv-mode
   :type git
   :host github
   :repo "emacsmirror/csv-mode"))
(straight-use-package
 '(lua-mode
   :type git
   :host github
   :repo "immerrr/lua-mode"))
(straight-use-package
 '(markdown-mode
   :type git
   :host github
   :repo "jrblevin/markdown-mode"))

;; Local Variables:
;; coding: utf-8-unix
;; fill-column: 80
;; require-final-newline: t
;; End:

;;; core-package.el ends here
