;;; core-package.el --- Core package specification -*- lexical-binding: t; -*-

;;; Commentary:

;; Provides the baseline API and tools to declaratively specify package loading

;;; Code:

;; Specify packages

(declare-function straight-use-package "straight")

(let* ((straight-current-profile 'core)
       ;; (straight-recipe-repositories nil)
       ;; (straight-recipe-overrides nil)
       (core-packages-progress
        (make-progress-reporter
         (format "Specifying packages for %s profile..."
                 (symbol-name straight-current-profile)))))
  ;; When straight-recipe-repositories is nil, all transitive dependencies must
  ;; be specified here, and order matters. Note that we currently don't do this
  ;; s it's apparently broken for many packages.

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
   '(avy
     :type git
     :host github
     :repo "abo-abo/avy"))
  ;; Dependency - we prefer avy for this functionality
  (straight-use-package
   '(ace-window
     :type git
     :host github
     :repo "abo-abo/ace-window"))
  (straight-use-package
   '(bui
     :type git
     :host github
     :repo "alezost/bui.el"))
  (straight-use-package
   '(posframe
     :type git
     :host github
     :repo "tumashu/posframe"))
  (straight-use-package
   '(cfrs
     :type git
     :host github
     :repo "Alexander-Miller/cfrs"))
  (straight-use-package
   '(el-get
     :type git
     :host github
     :repo "dimitri/el-get"))
  (straight-use-package
   '(epl
     :type git
     :host github
     :repo "cask/epl"))
  (straight-use-package
   '(ht
     :type git
     :host github
     :repo "wilfred/ht.el"))
  (straight-use-package
   '(let-alist
        :type git
        :host github
        :repo "emacs-straight/let-alist"))
  (straight-use-package
   '(pfuture
     :type git
     :host github
     :repo "Alexander-Miller/pfuture"))
  (straight-use-package
   '(pkg-info
     :type git
     :host github
     :repo "emacsorphanage/pkg-info"))
  (straight-use-package
   '(queue
     :type git
     :host github
     :repo "emacs-straight/queue"))
  (straight-use-package
   '(spinner
     :type git
     :host github
     :repo "Malabarba/spinner.el"))
  (straight-use-package
   '(page-break-lines
     :type git
     :host github
     :repo "purcell/page-break-lines"))

  ;; All the Icons
  (straight-use-package
   '(memoize
     :type git
     :host github
     :repo "skeeto/emacs-memoize"))
  (straight-use-package 
   '(all-the-icons
     :type git
     :host github
     :repo "domtronn/all-the-icons.el"))

  ;; Dashboard
  (straight-use-package
   '(emacs-dashboard
     :type git
     :host github
     :repo "emacs-dashboard/emacs-dashboard"))

  ;; Org-bullets
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

  ;; Exec path from shell
  (straight-use-package
   '(exec-path-from-shell
     :type git
     :host github
     :repo "purcell/exec-path-from-shell"))

  ;; Hydra
  (straight-use-package
   '(lv
     :type git
     :host github
     :repo "abo-abo/hydra"))
  (straight-use-package
   '(hydra
     :type git
     :host github
     :repo "abo-abo/hydra"))

  ;; Projectile
  (straight-use-package
   '(projectile
     :type git
     :host github
     :repo "bbatsov/projectile"))

  ;; Selectrum
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

  ;; Company
  (straight-use-package
   '(company
     :type git
     :host github
     :repo "company-mode/company-mode"))
  (straight-use-package
   '(company-prescient
     :type git
     :host github
     :repo "raxod502/prescient.el"))

  ;; Consult
  (straight-use-package
   '(consult
     :type git
     :host github
     :repo "minad/consult"))
  (straight-use-package
   '(flycheck
     :type git
     :host github
     :repo "flycheck/flycheck"))
  (straight-use-package
   '(consult-flycheck
     :type git
     :host github
     :repo "minad/consult"))

  ;; Marginalia
  (straight-use-package
   '(marginalia
     :type git
     :host github
     :repo "minad/marginalia"))

  ;; Embark
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

  ;; Flyspell-correct
  (straight-use-package
   '(flyspell-correct
     :type git
     :host github
     :repo "d12frosted/flyspell-correct"))

  ;; Solarized theme
  (straight-use-package
   '(solarized-theme
     :type git
     :host github
     :repo "bbatsov/solarized-emacs"))

  ;; Async
  (straight-use-package
   '(async
     :type git
     :host github
     :repo "jwiegley/emacs-async"))

  ;; Magit
  (straight-use-package
   '(transient
     :type git
     :host github
     :repo "magit/transient"))
  (straight-use-package
   '(with-editor
     :type git
     :host github
     :repo "magit/with-editor"))
  (straight-use-package
   '(git-commit
     :type git
     :host github
     :repo "magit/magit"))
  (straight-use-package
   '(magit
     :type git
     :host github
     :repo "magit/magit"))

  ;; Whitespace butler
  (straight-use-package
   '(ws-butler
     :type git
     :host github
     :repo "lewang/ws-butler"))

  ;; Expand region
  (straight-use-package
   '(expand-region
     :type git
     :host github
     :repo "magnars/expand-region.el"))

  ;; Smartparens
  (straight-use-package
   '(smartparens
     :type git
     :host github
     :repo "Fuco1/smartparens"))

  ;; Yasnippet
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

  ;; Undo-tree
  (straight-use-package
   '(undo-tree
     :type git
     :host gitlab
     :repo "tsc25/undo-tree"))

  ;; Flycheck
  (straight-use-package
   '(flycheck
     :type git
     :host github
     :repo "flycheck/flycheck"))

  ;; Which-key
  (straight-use-package
   '(which-key
     :type git
     :host github
     :repo "justbur/emacs-which-key"))

  ;; Transpose-frame
  (straight-use-package
   '(transpose-frame
     :type git
     :host github
     :repo "emacsorphanage/transpose-frame"))

  ;; Rainbow delimiters
  (straight-use-package
   '(rainbow-delimiters
     :type git
     :host github
     :repo "Fanael/rainbow-delimiters"))

  ;; CMake-mode
  (straight-use-package
   '(cmake-mode
     :type git
     :host github
     :repo "emacsmirror/cmake-mode"))

  ;; YAML-mode
  (straight-use-package
   '(yaml-mode
     :type git
     :host github
     :repo "yoshiki/yaml-mode"))

  ;; CSV-mode
  (straight-use-package
   '(csv-mode
     :type git
     :host github
     :repo "emacsmirror/csv-mode"))

  ;; Lua-mode
  (straight-use-package
   '(lua-mode
     :type git
     :host github
     :repo "immerrr/lua-mode"))

  ;; Markdown-mode
  (straight-use-package
   '(markdown-mode
     :type git
     :host github
     :repo "jrblevin/markdown-mode"))

  ;; LSP
  (straight-use-package
   '(lsp-mode
     :type git
     :host github
     :repo "emacs-lsp/lsp-mode"))
  (straight-use-package
   '(lsp-ui
     :type git
     :host github
     :repo "emacs-lsp/lsp-ui"))
  (straight-use-package
   '(consult-lsp
     :type git
     :host github
     :repo "gagbo/consult-lsp"))

  ;; Treemacs
  (straight-use-package
   '(treemacs
     :type git
     :host github
     :repo "Alexander-Miller/treemacs"))
  (straight-use-package
   '(treemacs-magit
     :type git
     :host github
     :repo "Alexander-Miller/treemacs"))
  (straight-use-package
   '(treemacs-all-the-icons
     :type git
     :host github
     :repo "Alexander-Miller/treemacs"))
  (straight-use-package
   '(treemacs-projectile
     :type git
     :host github
     :repo "Alexander-Miller/treemacs"))
  (straight-use-package
   '(treemacs-icons-dired
     :type git
     :host github
     :repo "Alexander-Miller/treemacs"))
  (straight-use-package
   '(lsp-treemacs
     :type git
     :host github
     :repo "emacs-lsp/lsp-treemacs"))

  ;; DAP-mode
  (straight-use-package
   '(dap-mode
     :type git
     :host github
     :repo "emacs-lsp/dap-mode"))

  ;; String-inflection
  (straight-use-package
   '(string-inflection
     :type git
     :host github
     :repo "akicho8/string-inflection"))

  ;; Editor-config
  (straight-use-package
   '(editor-config
     :type git
     :host github
     :repo "editorconfig/editorconfig-emacs"))

  (progress-reporter-done core-packages-progress))

;; Local Variables:
;; coding: utf-8-unix
;; fill-column: 80
;; require-final-newline: t
;; End:

;;; core-package.el ends here
