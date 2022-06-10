;;; core-package.el --- Core package specification -*- lexical-binding: t; -*-

;;; Commentary:

;; Declaratively specifies packages for our configuration

;;; Code:

;; Specify packages

(require 'straight)

(let ((default-directory (concat (file-name-directory load-file-name)
                                 "lisp")))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))
(require 'minmacs)
(require 'hgs-core)


(minmacs-as-core-packages
  (let ((core-packages-progress
         (make-progress-reporter
          (format "Specifying packages for %s profile..."
                  (symbol-name straight-current-profile)))))

    ;;   HOW STRAIGHT WORKS IN A NUTSHELL:
    ;;
    ;; `straight-use-package' clones, builds and adds the given package to
    ;; load-path. `straight-register-package' registers the recipe for that
    ;; given package, such that any future need for that package will be
    ;; affected.
    ;;
    ;; Straight will look in the `straight-recipe-repositories' list in order to
    ;; find default recipes for any given package. These repositories are
    ;; effectively Git mirror of recipe definitions. These recipe definitions
    ;; define things like what files to install (`:files') and where to get them
    ;; from (`:host' and `repo'). When you provide your own
    ;; `straight-use-package' recipe, straight will attempt to merge them, but
    ;; note that some parts won't be mergeable (for example `:flavor'). Note
    ;; that `straight-use-package' calls will attempt to fetch any transitive
    ;; dependencies automatically via the recipe mirrors. If you have a burning
    ;; need for a transitive dependency to be a specific version, make sure to
    ;; pin it prior to installing any dependent packages.
    ;;
    ;; In short there are two sources of state here: an implicit set of recipes
    ;; in the relevant Git mirrors of MELPA, ELPA etc., and the locations of the
    ;; packages themselves. All of this state is pinned in the lock files. We
    ;; prefer to only specify the minimum that's sensible: host and repository,
    ;; relying heavily on the Git recipe mirrors for things like installation
    ;; rules (`:files'). This isn't the best for reproducibility, but provided
    ;; that the recipe mirrors are alive, as are the package repos, and we are
    ;; using lockfiles, then we have a fairly high level of flexibility &
    ;; reliability for little trade-off in convenience for maintenance.
    ;;
    ;;   VERSIONING:
    ;;
    ;; Use `straight-pull-all' or `straight-pull-package' to update package
    ;; versions, then use `straight-freeze-versions' to update the lock files.
    ;; Use `straight-thaw-versions' to roll back to the versions provided in the
    ;; lock files.
    ;;
    ;;   MISC:
    ;;
    ;; When `straight-recipe-repositories' is nil, all transitive dependencies
    ;; must be specified here, and order matters. Note that we currently don't
    ;; do this as it's apparently broken for many packages.
    ;;
    ;; You can use `(minmacs-get-transitive-dependencies PACKAGE-NAME)' to
    ;; retrieve a list of transitive dependencies of a declared package. This is
    ;; useful for debugging package declaration ordering. Note that this won't
    ;; work if the package hasn't already been successfully loaded. Your package
    ;; specification should be ordered similarly, as order matters for
    ;; `straight-use-package' calls.
    ;;
    ;;   THOUGHTS:
    ;;
    ;; If you read all this an thought "well that's over-complicated", you are
    ;; correct. Emacs package management is pretty much a mess, and generally
    ;; people fail when trying to add reproducibility. Straight does an
    ;; admirable job, but has a bit too much implicit reliance on recipe mirrors
    ;; for my liking (why are MELPA recipes distinct from the Git repos in the
    ;; first place? I can't think of a single instance where you wouldn't want
    ;; to know *how* to install a package in the package repository itself). At
    ;; least it's in a better state than Vim, and the automation around lock
    ;; files/profiles is great.

    (straight-use-package
     '(f
       :type git
       :host github
       :repo "rejeep/f.el"
       :files ("f.el" "f-shortdoc.el")))
    (straight-use-package
     '(compat
       :type git
       :host github
       :repo "phikal/compat.el"))
    (straight-use-package
     '(vimrc-mode
       :type git
       :host github
       :repo "mcandre/vimrc-mode"))
    (straight-use-package
     '(tablist
       :type git
       :host github
       :repo "politza/tablist"))
    (straight-use-package
     '(pdf-tools
       :type git
       :host github
       :repo "vedang/pdf-tools"))
    (straight-use-package
     '(page-break-lines
       :type git
       :host github
       :repo "purcell/page-break-lines"))
    (straight-use-package
     '(dash
       :type git
       :host github
       :repo "magnars/dash.el"))
    (straight-use-package
     '(dockerfile-mode
       :type git
       :host github
       :repo "spotify/dockerfile-mode"))
    (straight-use-package
     '(protobuf-mode
       :type git
       :host github
       :repo "google/protobuf"
       :files ("editors/protobuf-mode.el")))
    (straight-use-package
     '(bazel
       :type git
       :host github
       :repo "bazelbuild/emacs-bazel-mode"))
    (straight-use-package
     '(meson-mode
       :type git
       :host github
       :repo "wentasah/meson-mode"))
    (straight-use-package
     '(jq-mode
       :type git
       :host github
       :repo "ljos/jq-mode"))
    (straight-use-package
     '(toml-mode
       :type git
       :host github
       :repo "dryman/toml-mode.el"))
    (straight-use-package
     '(dimmer
       :type git
       :host github
       :report "gonewest818/dimmer.el"))
    (straight-use-package
     '(avy
       :type git
       :host github
       :repo "abo-abo/avy"))
    (straight-use-package
     '(all-the-icons
       :type git
       :host github
       :repo "domtronn/all-the-icons.el"))
    (straight-use-package
     '(clang-format
       :type git
       :host github
       :repo "emacsmirror/clang-format"))
    (straight-use-package
     '(org-bullets
       :type git
       :host github
       :repo "sabof/org-bullets"))
    (straight-use-package
     '(exec-path-from-shell
       :type git
       :host github
       :repo "purcell/exec-path-from-shell"))
    (straight-use-package
     '(selectrum
       :type git
       :host github
       :repo "raxod502/selectrum"))
    (straight-use-package
     '(company
       :type git
       :host github
       :repo "company-mode/company-mode"))
    (straight-use-package
     '(consult
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
     '(yaml
       :type git
       :host github
       :repo "zkry/yaml.el"))
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
     '(tree-sitter
       :type git
       :host github
       :repo "ubolonton/emacs-tree-sitter"))
    (straight-use-package
     '(tree-sitter-langs
       :type git
       :host github
       :repo "ubolonton/tree-sitter-langs"))
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
    (straight-use-package
     '(restclient
       :type git
       :host github
       :repo "pashky/restclient.el"))
    (straight-use-package
     '(string-inflection
       :type git
       :host github
       :repo "akicho8/string-inflection"))
    (straight-use-package
     '(editor-config
       :type git
       :host github
       :repo "editorconfig/editorconfig-emacs"))
    (straight-use-package
     '(wgrep
       :type git
       :host github
       :repo "mhayashi1120/Emacs-wgrep"))
    (straight-use-package
     '(vterm
       :type git
       :host github
       :repo "akermu/emacs-libvterm"))
    (straight-use-package
     '(clang-format+
       :type git
       :host github
       :repo "SavchenkoValeriy/emacs-clang-format-plus"))
    (straight-use-package
     '(hydra
       :type git
       :host github
       :repo "abo-abo/hydra"))
    (straight-use-package
     '(flycheck
       :type git
       :host github
       :repo "flycheck/flycheck"))
    (straight-use-package
     '(vundo
       :type git
       :host github
       :repo "casouri/vundo"))
    (straight-use-package
     '(undo-fu
       :type git
       :host github
       :repo "emacsmirror/undo-fu"))
    (straight-use-package
     '(undo-fu-session
       :type git
       :host github
       :repo "emacsmirror/undo-fu-session"))
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
     '(embark-consult
       :type git
       :host github
       :repo "oantolin/embark"))
    (straight-use-package
     '(json-mode
       :type git
       :host github
       :repo "joshwnj/json-mode"))
    (straight-use-package
     '(magit
       :type git
       :host github
       :repo "magit/magit"))
    (straight-use-package
     '(libgit
       :type git
       :host github
       :repo "magit/libegit2"))
    (straight-use-package
     '(magit-libgit
       :type git
       :host github
       :repo "magit/magit"))
    (straight-use-package
     '(forge
       :type git
       :host github
       :repo "magit/forge"))
    (straight-use-package
     '(consult-flycheck
       :type git
       :host github
       :repo "minad/consult-flycheck"))
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
     '(treemacs-icons-dired
       :type git
       :host github
       :repo "Alexander-Miller/treemacs"))
    (straight-use-package
     '(lsp-treemacs
       :type git
       :host github
       :repo "emacs-lsp/lsp-treemacs"))
    (straight-use-package
     '(dap-mode
       :type git
       :host github
       :repo "emacs-lsp/dap-mode"))
    (straight-use-package
     '(docker
       :type git
       :host github
       :repo "Silex/docker.el"))
    (straight-use-package
     '(smartparens
       :type git
       :host github
       :repo "Fuco1/smartparens"))
    (straight-use-package
     '(dashboard
       :type git
       :host github
       :repo "emacs-dashboard/emacs-dashboard"))
    (straight-use-package
     '(org-contrib
       :includes (org)))
    (straight-use-package
     '(org-mime
       :type git
       :host github
       :repo "org-mime/org-mime"))
    (straight-use-package
     '(doom-modeline
       :type git
       :host github
       :repo "seagle0128/doom-modeline"))
    (straight-use-package
     '(auth-source-pass
       :type git
       :host github
       :repo "DamienCassou/auth-source-pass"))
    (straight-use-package
     '(password-store
       :type git
       :host github
       :repo "zx2c4/password-store"))
    (straight-use-package
     '(pass
       :type git
       :host github
       :repo "NicolasPetton/pass"))
    (straight-use-package
     '(erc-hl-nicks
       :type git
       :host github
       :repo "leathekd/erc-hl-nicks"))
    (straight-use-package
     '(message-templ
       :type git
       :host nil
       :repo "https://git.tethera.net/message-templ.git"
       :files ("message-templ.el")))
    (straight-use-package
     '(notmuch
       :type git
       :host nil
       :repo "https://git.notmuchmail.org/git/notmuch"
       :files ("emacs/*.el" "emacs/*.svg")))

    (progress-reporter-done core-packages-progress)))

(provide 'core-package)

;; Local Variables:
;; coding: utf-8-unix
;; fill-column: 80
;; require-final-newline: t
;; End:

;;; core-package.el ends here.
