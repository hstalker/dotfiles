;;; core-config.el --- Core emacs configurations -*- lexical-binding: t; -*-

;;; Commentary:

;; Performs base-line Emacs and package configuration upon which the end-user
;; can build.

;;; Code:

(require 'cl-lib)
(require 'files)
(require 'custom)

;; Just make clear that these variables and functions do, in fact, exist.
;; Perhaps we can restructure this configuration to make it use Emacs
;; require/provide etc. mechanisms instead in future, which would obviate the
;; need for this.
(declare-function use-package "use-package")

(defvar hgs-config-directory)
(defvar hgs-cache-directory)
(defvar hgs-data-directory)
(defvar hgs-project-directory)
(defvar hgs-org-directory)
(defvar hgs-is-bsd)
(defvar hgs-is-linux)
(defvar hgs-is-mac)
(defvar hgs-is-windows)
(defvar hgs-frame-customization-hook)
(defvar hgs-frame-customization-gui-hook)
(defvar hgs-frame-customization-tui-hook)

;; Personal functions

(defun hgs-indent-buffer ()
  "Indent the current buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(defun hgs-byte-compile-configuration ()
  "Byte compile our configuration scripts."
  (interactive)
  (byte-recompile-directory hgs-config-directory 0))

(defun hgs-reload-configuration ()
  "Reload our Emacs configuration."
  (interactive)
  (message "Reloading user configuration from %s..." user-init-file)
  (load user-init-file))

;; Periodically clearing most of the buffer list is pretty useful
(defun hgs-kill-other-buffers ()
  "Kill all other open buffers."
  (interactive)
  (mapc #'kill-buffer (delq (current-buffer) (buffer-list))))

;; Emacs likes to dump all your current state into `CUSTOM-FILE' arbitrarily.
;; Make it painless to kill this file from Emacs.
(defun hgs-delete-custom-file ()
  "Delete the currently set `CUSTOM-FILE' from disk."
  (interactive)
  (when custom-file
    (let ((custom-files
           (mapcar #'(lambda (x) (concat (file-name-sans-extension custom-file)
                                         x))
                   '(".el" ".elc"))))
      (mapcar #'(lambda (x)
                  (message "Deleting %s" x)
                  (delete-file x))
              custom-files))))

;; Some convenient functions for figuring out current monitor dimensions
(defun hgs-monitor-is-portrait ()
  "Predicate returning `t' if the current display monitor is of portrait
dimensions. This won't work as expected under daemon mode."
  (if (< (display-pixel-width) (display-pixel-height))
      t
    nil))

(defun hgs-monitor-is-horizontal ()
  "Predicate returning `t' if the current display monitor is wider than it is
tall. This won't work as expected under daemon mode."
  (not (hgs-monitor-is-portrait)))

;; Internal/built-in package configuration
;; Configure things defined in the core-most Emacs C code.
;; If you aren't sure where something goes, prefer to put it here instead of in
;; global scope.
(use-package emacs
  :no-require t ; Don't load `emacs.el' (obviously - it won't work)

  :init
  (defun hgs--setup-default-fonts (frame)
    "Setup default fonts for the given frame."
    (cl-flet ((get-font-family (lambda (font) (car font)))
              (get-font-size (lambda (font) (cadr font))))
      (let ((latin-font '("7x13" 12))
            (unicode-font '("DejaVu Sans" 12)))
        (set-face-attribute 'default frame
                            :family (get-font-family latin-font))
        (set-fontset-font t 'unicode
                          (font-spec :family (get-font-family unicode-font)
                                     :size (get-font-size unicode-font))
                          frame)
        (set-fontset-font t 'latin
                          (font-spec :family (get-font-family latin-font)
                                     :size (get-font-size latin-font))
                          frame 'prepend))))

  (defun hgs--setup-default-gui (frame)
    "Set default graphical UI for the given frame."
    (scroll-bar-mode -1)
    (menu-bar-mode -1)
    (tool-bar-mode -1)
    (modify-frame-parameters frame
                             '((fullscreen . nil)
                               (vertical-scroll-bar . nil)
                               (horizontal-scroll-bar . nil)))

    ;; HACK: Work around a bug in GUI emacs 26 causing rendering issues
    ;; Bug report: https://lists.gnu.org/r/bug-gnu-emacs/2018-04/msg00658.html
    (when (eq emacs-major-version 26)
      (modify-frame-parameters frame
                               '((inhibit-double-buffering . t)))))

  (defun hgs--setup-default-tui (frame)
    "Set default terminal UI for the given frame."
    ;; Do nothing for now
    frame)

  (add-hook 'hgs-frame-customization-hook #'hgs--setup-default-fonts -50)
  (add-hook 'hgs-frame-customization-gui-hook #'hgs--setup-default-gui -50)
  (add-hook 'hgs-frame-customization-tui-hook #'hgs--setup-default-tui -50)

  (defun hgs--suspend-frame ()
    "In a GUI environment, do nothing; otherwise `suspend-frame'."
    (interactive)
    (if (display-graphic-p)
        (message "suspend-frame disabled for graphical displays.")
      (suspend-frame)))

  :config
  ;; Set standard encodings
  (when (fboundp 'set-charset-priority)
    (set-charset-priority 'unicode))
  (prefer-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
  ;; We prefer to default to utf-8 with unix line endings
  (setq-default buffer-file-coding-system 'prefer-utf-8-unix)

  ;; We always want to use the short form for confirmation prompts
  (defalias 'yes-or-no-p 'y-or-n-p)

  ;; Change the string displayed in the frame title to be more informative
  (setq frame-title-format
        `((:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name))
                   "%b"))
          (:eval (if (buffer-modified-p)
                     "[*]"))
          " - "
          ,(user-login-name) "@" ,(system-name)))

  ;; Put game scores directories under cache
  (setq shared-game-score-directory
        (concat (file-name-as-directory hgs-cache-directory) "games/shared"))

  ;; Enable narrow-to-* family of functions, as they're disabled by default
  (put 'narrow-to-region 'disabled nil)
  (put 'narrow-to-defun 'disabled nil)
  (put 'narrow-to-page 'disabled nil)

  (bind-keys :map global-map
             ([remap suspend-frame] . hgs--suspend-frame))

  :custom
  (cursor-type 'bar "User a bar cursor rather than a box, as bar better suits
Emacs marking.")
  (enable-recursive-minibuffers t "Allow for minibuffer usage inside
minibuffers.")
  (visible-bell nil "Don't show visual bell.")
  (fill-column 80 "Set the fill column to a general standard of 80.")
  (indent-tabs-mode nil "We don't want to use tabs for indentation.")
  (tab-width 2 "Set the default tab width to 2")
  (inhibit-startup-message t "Don't show a startup message.")
  (inhibit-startup-echo-area-message nil "Don't show a startup message in the
echo area.")
  (inhibit-default-init t "Don't load the default library.")
  (initial-scratch-message nil "Don't show an initial message in the scratch
buffer.")
  (auto-save-list-file-prefix (concat
                               (file-name-as-directory hgs-cache-directory)
                               "auto-save-list")
                              "Put autosaves in our cache directory.")
  (sentence-end-double-space nil "Tell Emacs that we don't use double spacing
for sentences.")
  (bidi-paragraph-direction 'left-to-right "We don't use Emacs for
non-left-to-right text, so this gives a nice performance boost.")
  (ring-bell-function #'ignore "We don't want bell audio.")
  (history-delete-duplicates
   t
   "Make sure we don't have duplicates in the command history.")
  (display-buffer-alist
   ;; Attempt to tame Emacs' tendency towards randomly replacing/creating
   ;; windows for predictability's sake. Generally you want this to be as simple
   ;; as possible, to allow for maximum flexibility at usage time.

   ;; Note: Be very careful about this structure. Breaking it breaks Emacs'
   ;; multiplexing.

   ;; General Structure:
   ;; (REGULAR-EXPRESSION-STRING
   ;;  ORDERED-LIST-OF-WINDOW-SELECTION-FUNCTIONS
   ;;  OPTIONS...)
   `(("^\\*\\([Hh]elp\\|[Cc]ustom\\|[Ff]aces\\).*"
      (display-buffer-reuse-mode-window
       display-buffer-in-side-window)
      (window-width . 0.25)
      ;; Would like to make this dynamic and based on monitor proportions, but
      ;; this won't work properly under daemon mode, and we can't really pass in
      ;; functions.
      (side . bottom)
      (slot . 0))
     ("^\\*\\([Cc]ompilation\\|[Cc]ompile-[Ll]og\\|[Ww]arnings\\).*"
      (display-buffer-reuse-mode-window
       display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . -1)
      (inhibit-same-window . t))
     ("^\\*[Mm]essages.*"
      (display-buffer-reuse-mode-window
       display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 1)
      (inhibit-same-window . t))
     ("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
      ()
      (window-parameters (mode-line-format . none))))
   "Custom overrides for window placement of specific buffers.")
  (display-buffer-base-action
   ;; Defaults to make Emacs let us control most of the window layout. This
   ;; makes Emacs prefer to use existing windows or the same window rather
   ;; than creating new splits by default.
   '((display-buffer-reuse-window
      display-buffer-reuse-mode-window
      display-buffer-same-window
      display-buffer-in-previous-window)
     . ())
   "Default action for window placement in order to make Emacs multiplexing more
predictable."))

;; Only needed for stopping some Emacs games from littering configuration
;; directory
(use-package gamegrid
  :config
  (setq gamegrid-user-score-file-directory
        (concat (file-name-as-directory hgs-cache-directory) "games/gamegrid")))

(use-package help
  :config
  (turn-on-visual-line-mode)

  :custom
  (help-window-select
   'always
   "We always want to focus the help window once we open it."))

(use-package compile
  :config
  ;; Hopefully fixes the compilation colorization
  (defun hgs--filter-compilation-buffer ()
    "Allows color escape codes to work better inside the compilation buffer for
a small performance hit, and forcibly hardwrap lines if they get too long."
    (read-only-mode)
    (ansi-color-apply-on-region compilation-filter-start (point))
    ;; Forcibly hard wrap lines if they get extremely long
    (save-excursion
      (goto-char compilation-filter-start)
      (while (let ((clip-length 400))
               (re-search-forward
                (format "\\(.\\{%d\\}\\).*?\\(\"\\)" clip-length)
                nil 'noerror))
        (replace-match "\\1\n\\2")))
    (read-only-mode))
  (add-hook 'compilation-filter-hook #'hgs--filter-compilation-buffer)

  ;; Make setting compile command safe via local variables
  (put 'compile-command 'safe-local-variable #'stringp)

  :custom
  (compilation-scroll-output
   'first-error
   "Follow compilation buffer output until first error.")
  (compilation-skip-threshold
   2
   "Don't stop scrolling on anything less than an error.")
  (compilation-ask-about-save t "Ask which buffers to save before compiling."))

(use-package gud)

(use-package ediff
  :custom
  (ediff-window-setup-function
   #'ediff-setup-windows-plain
   "User a single frame for ediff rather than multiple.")
  (ediff-split-window-function
   #'split-window-vertically
   "Use vertical splits for ediff."))

;; Enable xterm mouse mode by default if running in the terminal
(use-package xt-mouse
  :if (not (display-graphic-p))
  :hook
  ((prog-mode text-mode) . xterm-mouse-mode))

(use-package simple
  :diminish
  line-number-mode
  column-number-mode
  auto-fill-mode

  :hook
  (((prog-mode text-mode) . line-number-mode)
   ((prog-mode text-mode) . column-number-mode)
   ((text-mode) . turn-on-auto-fill)))

(if (version<= emacs-version "26.0.50")
    (use-package linum
      :diminish
      global-linum-mode
      linum-mode

      :hook
      ((prog-mode text-mode) . linum-mode)) ; Slow & old
  ;; Fast & shiny
  (use-package display-line-numbers
    :diminish
    global-display-line-numbers-mode
    display-line-numbers-mode

    :hook
    ((prog-mode text-mode) . display-line-numbers-mode)))

;; Displays the whitespace
(use-package whitespace
  :diminish
  global-whitespace-mode
  whitespace-mode

  :commands
  global-whitespace-mode
  whitespace-mode

  :functions
  hgs--reset-whitespace-mode-local-hack

  :init
  (defun hgs--reset-whitespace-mode ()
    "Turns `WHITESPACE-MODE' off and then on to allow changes in variables
to propagate."
    (whitespace-mode -1)
    (whitespace-mode +1))

  (defun hgs--reset-whitespace-mode-local-hack ()
    "Reset `WHITESPACE-MODE' after local variables are set to allows for them
to be taken into account inside `WHITESPACE-MODE'."
    (add-hook 'hack-local-variables-hook
              #'hgs--reset-whitespace-mode
              nil 'local))

  :hook
  ((prog-mode text-mode) . hgs--reset-whitespace-mode-local-hack)

  :config
  (put 'whitespace-line-column 'safe-local-variable #'integerp)

  :custom
  (whitespace-line-column nil "Max line length for whitespace mode. Uses
`fill-column' when set to nil.")
  (whitespace-style
   '(face
     trailing
     lines-tail
     space-after-tab
     space-before-tab)
   "What whitespace should we visualize?"))

(use-package apropos
  :custom
  (apropos-do-all t "Make apropos commands search more extensively (as if
given a prefix argument by default."))

(use-package files
  :custom
  (tags-revert-without-query t "Don't prompt me to reload the current TAGS
file when it changes on disk.")
  (large-file-warning-threshold
   (* 1024 1024 200)
   "Increase the default large file warning threshold to 200MB.")

  (auto-save-file-name-transforms
   `((".*" ,(concat (file-name-as-directory hgs-cache-directory)) "backup")
     t)
   "Put autosaves in the cache directory.")
  (backup-directory-alist
   `((".*" . ,(concat (file-name-as-directory hgs-cache-directory) "backup")))
   "Put backups in the cache directory.")
  (backup-by-copying t "Always copy rather than symlink for backups."))


(use-package recentf
  :custom
  (recentf-save-file
   (concat (file-name-as-directory hgs-cache-directory) "recentf")
   "Place the recentf cache into our cache directory."))

(use-package vc
  :bind-keymap
  ("C-x v" . vc-prefix-map)

  :bind
  (:map vc-prefix-map
        ("=" . ediff-revision))

  :custom
  (vc-follow-symlinks t "Make the version control functionality automatically
follow symlinks to files potentially outside of the VCS (or inside another)."))

(use-package dired
  :init
  ;; Dired tends to clutter the buffer list quite heavily.
  (defun hgs-kill-dired-buffers ()
    "Kill all open Dired buffers."
    (interactive)
    (mapc (lambda (buffer)
            (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
              (kill-buffer buffer)))
          (buffer-list)))

  (defun hgs--dired-up-directory-clean ()
    "Stop Dired from creating a new buffer when you go up a directory."
    (interactive)
    (find-alternate-file ".."))

  :commands
  dired

  :bind
  ((:map global-map
         ("C-x d" . dired))
   (:map dired-mode-map
         ("^" . hgs--dired-up-directory-clean)))

  :config
  ;; Emacs disables this by default, but it's the only real way to use dired w/o
  ;; buffer spam.
  (put 'dired-find-alternate-file 'disabled nil)

  :custom
  (dired-listing-switches "-alh" "Tell Dired to use human-readable units.")
  (dired-recursive-copies 'always "Tell Dired to default to performing
recursive copies.")
  (dired-dwim-target t "Do what I mean when I have multiple Dired windows
open."))

(use-package winner
  :diminish
  winner-mode

  :commands
  winner-mode
  winner-undo
  winner-redo

  :hook
  ((prog-mode text-mode special-mode) . winner-mode))

(use-package desktop
  :config
  (setq desktop-dirname (concat (file-name-as-directory hgs-data-directory)
                                "desktop"))

  :custom
  (desktop-base-file-name "autosave" "Name of the desktop save file.")
  (desktop-base-lock-name "autosave-lock" "Name of the lock for desktop
package."))

(use-package tramp
  :custom
  (tramp-auto-save-directory
   (concat (file-name-as-directory hgs-cache-directory)
           "tramp-auto-save")
   "Directory to place autosaves when editing via Tramp.")
  (tramp-backup-directory-alist
   backup-directory-alist
   "Put Tramp backups in the same place as local backups.")
  (tramp-persistency-file-name
   (concat (file-name-as-directory hgs-cache-directory)
           "tramp-persistency.el")
   "Put the Tramp persistency file in the cache."))

(use-package eshell
  :functions
  eshell/pwd
  vc-git-branches

  :init
  (defun hgs--eshell-prompt-function ()
    "Function that determines the eshell prompt to display.
`eshell-prompt-function' must be set to this to be activated."
    (require 'subr-x)
    (require 'vc-git)
    (let ((path (file-name-nondirectory (eshell/pwd))))
      (concat
       (format (propertize "[%s:%s]")
               (propertize user-login-name 'face '(:inherit eshell-prompt))
               (propertize "eshell" 'face '(:inherit eshell-prompt)))
       (format (propertize "<%s>" 'face '(:foreground "red"))
               path)
       (when-let ((branch (and (executable-find "git")
                               (car (vc-git-branches)))))
         (format (propertize "(%s)" 'face '(:foreground "blue"))
                 branch))
       (propertize "λ" 'face '(:foreground "teal"))
       " ")))

  :custom
  (eshell-buffer-maximum-lines
   20000
   "Truncate eshell buffers to something reasonable.")
  (eshell-highlight-prompt nil "We don't need prompt highlighting.")
  (eshell-hist-ignoredups t "No duplicates in shell history.")
  (eshell-history-size 2048 "Cap our history size to something reasonable.")
  (eshell-plain-echo-behavior t "Make `echo' imitate shell echo.")
  (eshell-prompt-function
   #'hgs--eshell-prompt-function
   "Setup my prompt to something useful.")
  (eshell-prompt-regexp ".+^λ " "Tell Emacs how to find prompts in the buffer.")
  (eshell-scroll-to-bottom-on-input
   'this
   "We want to scroll to the bottom on input.")
  (eshell-scroll-to-bottom-on-output
   nil
   "We don't want to scroll to bottom on output."))


(use-package url
  :custom
  (url-cache-directory
   (concat (file-name-as-directory hgs-cache-directory)
           "url")
   "Put the url package's cache directory where we would expect.")
  (url-configuration-directory
   (concat (file-name-as-directory hgs-data-directory)
           "url")
   "Put the url package's configuration directory in the data directory."))

(use-package bookmark
  :custom
  (bookmark-default-file
   (concat (file-name-as-directory hgs-data-directory)
           "bookmarks")
   "Put bookmarks in the data directory."))

(use-package custom
  :custom
  (custom-theme-directory
   (concat (file-name-as-directory hgs-config-directory)
           "themes")
   "Expect custom themes from our configuration directory."))

(use-package python
  :defines
  python-indent-guess-indent-offset

  :commands
  python-mode

  :mode
  (("\\.py\\'" . python-mode))

  :custom
  (python-indent-offset 2 "We prefer to default indent in python to 2 space.")
  (python-indent-guess-indent-offset
   t
   "Guess indent offset and set it appropriately."))

(use-package org
  :commands
  org-mode
  org-store-link
  org-capture
  org-agenda
  org-clock-goto

  :mode
  ("\\.org\\'" . org-mode)

  :bind
  (:prefix "C-c o"
           :prefix-map hgs--org-prefix-map
           :prefix-docstring "Org commands"
           ("l" . org-store-link)
           ("c" . org-capture)
           ("a" . org-agenda)
           ("j" . org-clock-goto))

  :custom
  (org-directory
   (file-name-as-directory hgs-org-directory)
   "Base path to store org files inside by default.")
  (org-default-notes-file
   (concat (file-name-as-directory hgs-org-directory) "notes.org")
   "Put org notes into the appropriate file & directory by default.")
  (org-agenda-files
   `(,(file-name-as-directory hgs-org-directory))
   "Directory to search for matching org files for agenda.")
  (org-todo-keywords
   '((sequence
      "TODO(t)" "INPROGRESS(p)" "BLOCKED(b)" ; Non-terminal states
      "|"
      "DONE(d)" "CANCELLED(c)")) ; Terminal states
   "Sequence of states for org-todo entries.")
  (org-capture-templates
   `(("n" "Notes" entry
      (file
       (lambda ()
         org-default-notes-file))
      "* NOTES %? %^G\n%U" :empty-lines 1)
     ("t" "Todo" entry
      (file
       (lambda ()
         (concat (file-name-as-directory org-directory) "todo.org")))
      "* TODO %? %^G\n%U" :empty-lines 1)
     ("s" "Scheduled Todo" entry
      (file
       (lambda ()
         (concat (file-name-as-directory org-directory) "todo.org")))
      "* TODO %? %^G\nSCHEDULED: %^t\n%U" :empty-lines 1)
     ("d" "Deadline" entry
      (file
       (lambda ()
         (concat (file-name-as-directory org-directory) "todo.org")))
      "* TODO %? %^G\nDEADLINE: %^t" :empty-lines 1))
   "Basic entry templates for org-capture.")
  (org-babel-load-languages
   `((awk . t)
     (C . t)
     (calc . t) ; Emacs calc
     (css . t)
     (ebnf . t)
     (emacs-lisp . t)
     (fortran . t)
     (gnuplot . t)
     (groovy . t)
     (screen . t)
     (dot . t)
     (haskell . t)
     (java . t)
     (js . t)
     (latex . t)
     (lisp . t)
     (lua . t)
     (matlab . t)
     (ocaml . t)
     (org . t)
     (python . t)
     (R . t)
     (ruby . t)
     (sass . t)
     (scheme . t)
     (sed . t)
     (shell . t)
     (sql . t)
     (sqlite . t)
     (vala . t))
   "The allowed languages to load org-babel support for by default.")
  (org-log-done 'time "Insert a time-stamp when a task moves to DONE state.")
  (org-auto-align-tags t "Keep tags aligned when modifying headlines.")
  (org-startup-align-all-tables
   nil
   "Don't automatically align all tables when visiting a file.")
  (org-startup-indented t "Use virtual indentation by default.")
  (org-startup-folded t "Default to showing overview of org buffer.")
  (org-startup-with-inline-images t "Enable inline image display by default.")
  (org-adapt-indentation nil "Stop org from indenting to match heading level.")
  (org-hide-leading-stars t "Remove redundant start count.")
  (org-hide-emphasis-markers t "Only show the markers when we edit them.")
  (org-blank-before-new-entry
   '((heading . nil)
     (plain-list-item . nil))
   "Customize behavior with respect to adding new lines on new entries.")
  (org-cycle-separator-lines
   1
   "Maintain all visible empty lines whilst cycling visibility.")
  (org-ellipsis "▼" "The ellipsis symbol to show for folds and the like."))


(use-package org-indent
  :after org

  :diminish
  org-indent-mode

  :commands
  org-indent-mode

  :custom
  (org-indent-indentation-per-level
   2
   "Reduce indentation in `org-indent' mode."))

;; Personal lisp packages

;; HERE

;; Third-party package configuration

(use-package org-bullets
  :after org

  :diminish
  org-bullets-mode

  :commands
  org-bullets-mode

  :hook
  ((org-mode) . org-bullets-mode)

  :custom
  (org-bullets-bullet-list
   '(;; Large
     "◉"
     "○"
     "✸"
     "✿"
     ;; ♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀ ◆ ◖ ▶
     ;; Small
     ;; ► ★ ▸
     )
   "Unicode bullets to use."))

(use-package clang-format+
  :commands
  clang-format+-mode

  :init
  (use-package clang-format)

  :hook
  ((c-mode c++-mode objc-mode) . clang-format+-mode)

  :custom
  (clang-format+-context
   'modification
   "Only reformat the modified lines. Prevents unnecessary changes in PRs.")
  (clang-format+-offset-modified-region 0 "")
  (clang-format+-always-enable
   nil
   "If we can't find a .clang-format, then we should not enable this
automation."))

(use-package exec-path-from-shell
  :commands
  exec-path-from-shell-initialize

  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  (when (daemonp)
    (exec-path-from-shell-initialize))

  :custom
  (exec-path-from-shell-variables
   '(;; System-defined variables
     "DISPLAY"
     "SSH_ASKPASS"
     "SSH_AUTH_SOCK"
     "SSH_AGENT_PID"
     "SSH_CONNECTION"
     "SSH_CLIENT"
     "SSH_TTY"
     "GPG_TTY"
     "RXVT_SOCKET"
     "WINDOWID"
     "XAUTHORITY"
     "DBUS_SESSION_BUS_ADDRESS"
     "SHELL"
     "PATH"
     "LD_LIBRARY_PATH"
     "CPATH"
     "MANPATH"
     "INFOPATH"
     "FPATH"
     "NAME"
     "EMAIL"
     "LANG"
     "LC_ALL"
     ;; User-defined variables
     )
   "All the shell variables Emacs should be attempting to source."))

(use-package hydra)

(use-package projectile
  :commands
  projectile-mode

  :config
  (projectile-mode t)

  ;; Make setting the projectile build-related variables via local variables
  ;; safe.
  (put 'projectile-project-configure-cmd 'safe-local-variable #'stringp)
  (put 'projectile-project-compilation-cmd 'safe-local-variable #'stringp)
  (put 'projectile-project-test-cmd 'safe-local-variable #'stringp)
  (put 'projectile-project-run-cmd 'safe-local-variable #'stringp)

  :bind-keymap
  ("C-c p" . projectile-command-map)

  :custom
  (projectile-completion-system
   'default
   "Use Selectrum (or rather: `completing-read') as the completion backend.")
  (projectile-project-search-path
   `(,hgs-project-directory ,hgs-user-directory)
   "Where should projectile search?")
  (projectile-known-projects-file
   (concat (file-name-as-directory hgs-cache-directory)
           "projectile-bookmarks.eld")
   "Where to cache known projects.")
  (projectile-use-git-grep nil "Don't use git-grep over other tools.")
  (projectile-cache-file
   (concat (file-name-as-directory hgs-cache-directory)
           "projectile.cache")
   "Place the projectile cache file into our cache directory.")
  (projectile-other-file-alist
   '(;; General C/C++ extensions
     ("C" . ("H")) ("H" . ("C"))
     ("cpp" . ("h" "hpp" "ipp"))
     ("ipp" . ("h" "hpp" "cpp"))
     ("hpp" . ("h" "ipp" "cpp" "cc"))
     ("cxx" . ("h" "hxx" "ixx"))
     ("ixx" . ("h" "hxx" "cxx"))
     ("hxx" . ("h" "ixx" "cxx"))
     ("c" . ("h"))
     ("m" . ("h"))
     ("mm" . ("h"))
     ("h" . ("c" "cc" "cpp" "ipp" "hpp" "cxx" "ixx" "hxx" "m" "mm"))
     ("cc" . ("h" "hh" "hpp"))
     ("hh" . ("cc"))

     ;; OCaml extensions
     ("ml" . ("mli"))
     ("mli" . ("ml" "mll" "mly"))
     ("mll" . ("mli"))
     ("mly" . ("mli"))
     ("eliomi" . ("eliom"))
     ("eliom" . ("eliomi"))

     ;; Typical vertex & fragment shader language extensions
     ("vert" . ("frag"))
     ("frag" . ("vert"))

     ;; Handle files with no extension
     (nil . ("lock" "gpg"))
     ("lock" . (""))
     ("gpg" . ("")))
   "Alist of extensions for switching to file with the same name, using other
extensions based on the extension of the current file."))

(use-package selectrum
  :diminish
  selectrum-mode

  :init
  (selectrum-mode +1))

(use-package prescient
  :after
  selectrum

  :diminish
  prescient-persist-mode

  :commands
  prescient-persist-mode

  :init
  (prescient-persist-mode +1)

  :custom
  (prescient-history-length
   100
   "Number of recently selected candidates to show at the top of the list.")
  (prescient-frequency-decay
   0.997
   "How much to decrease candidates' priorities for subsequent non-selections.")
  (prescient-save-file
   (concat (file-name-as-directory hgs-cache-directory)
           "prescient-statistics")
   "Where to save Prescient statistics to on disk.")
  (prescient-filter-method
   '(literal
     regexp
     initialism)
   "List of algorithms to use for filtering candidates. This is the power of
Prescient")
  (prescient-filter-alist
   '((literal . prescient-literal-regexp)
     (literal-prefix . prescient-literal-prefix-regexp)
     (initialism . prescient-initials-regexp)
     (regexp . prescient-regexp-regexp)
     (fuzzy . prescient-fuzzy-regexp)
     (prefix . prescient-prefix-regexp)
     (anchored . prescient-anchored-regexp))
   "Alist for associating symbols with custom filter methods.")
  (prescient-sort-full-matches-first t "Put full matches ahead of partial in the
list.")
  (prescient-use-char-folding t "Whether the literal and literal-prefix
filters use character folding.")
  (prescient-use-case-folding 'smart "Whether filters should be
case-insensitive. Smart disables case insensitivity when upper case is used."))

(use-package selectrum-prescient
 :after
 selectrum
 prescient

 :diminish
 selectrum-prescient-mode

 :commands
 selectrum-prescient-mode

 :init
 (selectrum-prescient-mode +1)

 :custom
 (selectrum-prescient-enable-filtering t "Enable filtering for selectrum.")
 (selectrum-prescient-enable-sorting t "Enable sorting for selectrum."))

(use-package company-prescient
 :after
 company
 prescient

 :diminish
 company-prescient-mode

 :commands
 company-prescient-mode

 :init
 (company-prescient-mode +1)

 :custom
 (company-prescient-sort-length-enable nil "Don't resort company's already
partially sorted lists by length, as this ruins the sort order."))

(use-package consult
  :after
  projectile                            ; For `projectile-project-root'

  :demand t

  :functions
  consult-register-window

  :bind
  (:map global-map
        ([remap yank-pop] . consult-yank-pop)
        ([remap apropos-command] . consult-apropos)
        ("C-h M" . consult-man)
        ("C-c H" . consult-history)
        ("M-s e" . consult-isearch)
        ("C-x K" . consult-kmacro)
        ("C-x r M-\"" . consult-register-load)
        ("C-x r M-'" . consult-register-store)
        ([remap bookmark-jump] . consult-bookmark))
  (:map ctl-x-map
        ([remap repeat-complex-command] . consult-complex-command)
        ([remap switch-to-buffer] . consult-buffer)
        ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
        ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame))
  (:map goto-map
        ("g" . consult-goto-line)
        ("M-g" . consult-goto-line)
        ("e" . consult-compile-error)
        ("o" . consult-outline)
        ("m" . consult-mark)
        ("k" . consult-global-mark)
        ("i" . consult-imenu)
        ("I" . consult-project-imenu))
  (:map search-map
        ("f" . consult-find)
        ("L" . consult-locate)
        ("g" . consult-grep)
        ("G" . consult-git-grep)
        ("r" . consult-ripgrep)
        ("l" . consult-line)
        ("m" . consult-multi-occur)
        ("k" . consult-keep-lines)
        ("u" . consult-focus-lines))
  (:map isearch-mode-map
        ([remap isearch-edit-string] . consult-isearch)
        ("M-s l" . consult-line))

  :config
  ;; Add thin lines, sorting and hide the mode line of the register preview
  ;; window
  (advice-add #'register-preview :override #'consult-register-window)

  :custom
  (xref-show-xrefs-function #'consult-xref "Use Consult for xref.")
  (xref-show-definitions-function #'consult-xref "Use Consult for xref.")
  (register-preview-delay 0 "Set no delay for the register preview for speed.")
  (register-preview-function
   #'consult-register-format
   "Use Consult for register preview.")
  (consult-narrow-key "<" "Specify the key used for explicit narrowing.")
  (consult-preview-key 'any "Trigger Consult previews with any key press.")
  (consult-project-root-function
   #'projectile-project-root
   "Use Projectile for finding the project root."))

(use-package consult-flycheck
 :after
 consult
 flycheck

 :commands
 consult-flycheck

 :bind
 (:map flycheck-command-map
       ("!" . consult-flycheck)))

(use-package marginalia
  :after
  selectrum

  :commands
  marginalia-mode

  :diminish
  marginalia-mode

  :bind
  (:map global-map
        ("M-A" . marginalia-cycle))
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle))

  :init
  (marginalia-mode +1)

  :custom
  (marginalia-annotators
   '(marginalia-annotators-heavy
     marginalia-annotators-light
     nil)
   "Prefer richer, heavier annotations over lighter alternatives."))

(use-package embark
  :defines
  embark-file-map

  :bind
  (:map global-map
        ("C-c e a" . embark-act))
  (:map minibuffer-local-map
        ("C-o" . embark-act)
        ("C-S-o" . embark-act-noexit))
  (:map embark-file-map
        ("j" . dired-jump)))

(use-package embark-consult
  :after
  embark
  consult

  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))

(use-package solarized-theme
  :config
  (load-theme 'solarized-light 'no-confirm)

  (defun hgs--solarized-tui-bg-removal (frame)
    "Disable background theming if in terminal as this causes breakage. This
means the terminal itself must be appropriately themed. Be careful about
not doing this if we are in daemon mode though, since it'll break GUI
emacsclient (invalid argument stringp errors)."
    (modify-frame-parameters frame '((background-color . nil))))

  ;; Add this at a high depth so that it runs last
  (add-hook 'hgs-frame-customization-tui-hook
            #'hgs--solarized-tui-bg-removal
            50)

  :custom
  (solarized-distinct-fringe-background t "Make the fringe stand out.")
  (solarized-use-variable-pitch
   nil
   "Don't change the font for some headings and titles.")
  (solarized-high-contrast-mode-line t "Make the mode line high contrast.")
  (solarized-use-less-bold t "Use less bolding.")
  (solarized-use-more-italic t "Use more italics.")
  (solarized-emphasize-indicators nil "Use less colors for indicators.")
  (solarized-scale-org-headlines
   nil
   "Don't change the size of org-mode headlines."))

(use-package async
  :init
  (setq async-byte-compile-log-file
        (concat (file-name-as-directory hgs-data-directory)
                "async-bytecomp.log"))

  :hook
  ((after-init . async-bytecomp-package-mode)
   (dired-mode . dired-async-mode)))

(use-package transient
  :custom
  (transient-levels-file
   (concat (file-name-as-directory hgs-cache-directory)
           "transient/levels.el")
   "Where to place the Transient levels cache file.")
  (transient-values-file
   (concat (file-name-as-directory hgs-cache-directory)
           "transient/values.el")
   "Where to place the Transient values cache file.")
  (transient-history-file
   (concat (file-name-as-directory hgs-cache-directory)
           "transient/history.el")
   "Where to place the Transient history cache file."))

(use-package magit
  :after transient

  :commands
  magit-status
  magit-dispatch
  magit-blame
  magit-stage
  magit-unstage
  magit-pull
  magit-push
  magit-clone
  magit-reset
  magit-fetch
  magit-reflog
  magit-commit
  magit-revert
  magit-stash

  :bind
  (:map global-map
        ("C-x g" . magit-status)
        ("C-x M-g" . magit-dispatch)))

;; Trim whitespace on touched lines only automatically when saving
(use-package ws-butler
  :diminish
  ws-butler-global-mode
  ws-butler-mode

  :commands
  ws-butler
  ws-butler-mode
  ws-butler-global-mode

  :hook
  ((text-mode prog-mode) . ws-butler-mode)

  :custom
  (ws-butler-keep-whitespace-before-point
   nil
   "Delete whitespace before point."))

(use-package expand-region
  :commands
  er/expand-region

  :bind
  (:map global-map
        ("C-=" . er/expand-region)))

(use-package smartparens
  :diminish
  smartparens-mode
  smartparens-strict-mode
  smartparens-global-mode
  smartparens-global-strict-mode

  :commands
  smartparens-mode
  smartparens-strict-mode
  smartparens-global-mode
  smartparens-global-strict-mode

  :bind
  (:map smartparens-mode-map
        ("C-M-k" . sp-kill-sexp)
        ("C-M-<backspace>" . sp-backward-kill-sexp)
        ("C-M-f" . sp-forward-sexp)
        ("C-M-b" . sp-backward-sexp)
        ("C-M-n" . sp-up-sexp)
        ("C-M-d" . sp-down-sexp)
        ("C-M-u" . sp-backward-up-sexp)
        ("C-M-p" . sp-backward-down-sexp)
        ("C-M-w" . sp-copy-sexp)
        ("C-M-s" . sp-splice-sexp)
        ("C-M-r" . sp-splice-sexp-killing-around)
        ("C-)" . sp-forward-slurp-sexp)
        ("C-}" . sp-forward-barf-sexp)
        ("C-(" . sp-backward-slurp-sexp)
        ("C-{" . sp-backward-barf-sexp)
        ("C-M-)" . hgs-sp-wrap-pair-prompt)
        ("C-M-(" . sp-unwrap-sexp)
        ("M-S" . sp-split-sexp)
        ("M-J" . sp-join-sexp)
        ("C-M-t" . sp-transpose-sexp))

  :init
  (require 'smartparens-config)
  (defun hgs-sp-wrap-pair-prompt (arg)
    "Prompt for a character `ARG' to wrap the selection with as a pair of
delimiters."
    (interactive "sEnter a pair character: \n")
    (declare-function sp-wrap-with-pair "smartparens")
    (sp-wrap-with-pair arg))

  :hook
  ((prog-mode . smartparens-mode)))

(use-package yasnippet
  :diminish
  yas-global-mode
  yas-minor-mode

  :commands
  yas-global-mode
  yas-minor-mode
  snippet-mode

  :functions
  yas-reload-all

  :mode
  (("\\.yasnippet\\'" . snippet-mode)
   ("\\.yas\\'" . snippet-mode))

  :hook
  ((prog-mode text-mode) . yas-minor-mode)

  :config
  ;; Needed to force Emacs to load up all snippets given in our personal
  ;; snippet directories
  (yas-reload-all)

  :custom
  (yas-snippet-dirs
   `(,(concat (file-name-as-directory hgs-config-directory)
              "snippets"))
   "Where to find snippet definitions."))

(use-package yasnippet-snippets
  :after yasnippet

  :config
  (yasnippet-snippets-initialize))

(use-package company
  :hook
  (prog-mode . company-mode)
  (org-mode . company-mode)

  :custom
  (company-backends
   '((company-keywords
      company-capf
      company-files
      company-etags
      company-cmake
      company-ispell
      company-yasnippet
      company-dabbrev-code
      company-abbrev))
   "Set default company backends to use.")
  (company-echo-delay 0.5 "How long to wait before echoing.")
  (company-idle-delay 0.5 "How long to wait before offering completion.")
  (company-show-numbers t "Number the completion options.")
  (company-tooltip-limit 20 "Cap the number of candidates on display.")
  (company-minimum-prefix-length
   2
   "Minimum length of prefix before completion.")
  (company-tooltip-align-annotations t "Align candidates to the right.")
  (company-tooltip-flip-when-above
   nil
   "Don't invert navigation direction when near the bottom of the page.")
  (company-dabbrev-downcase nil "Don't downcase return value of dabbrev."))

(use-package undo-tree
  :diminish
  global-undo-tree-mode
  undo-tree-mode

  :commands
  global-undo-tree-mode
  undo-tree-mode
  undo-tree-undo
  undo-tree-redo
  undo-tree-visualize

  :hook
  ((prog-mode text-mode) . undo-tree-mode))

(use-package flycheck
  :commands
  flycheck-mode

  :bind-keymap
  ("C-c !" . flycheck-keymap-prefix)

  :hook
  ((prog-mode) . flycheck-mode))

(use-package flyspell
  :if (executable-find "aspell")

  :commands
  flyspell-mode
  flyspell-prog-mode

  :hook
  (((text-mode) . flyspell-mode)
   ((prog-mode) . flyspell-prog-mode))

  :custom
  (ispell-program-name "aspell" "Spellcheck program to use.")
  (ispell-extra-args
   '("--sug-mode=ultra" "--lang=en_US")
   "Change the default lookup mode for performance, and force language to
American English."))

(use-package flyspell-correct
  :after
  flyspell

  :bind
  (:map flyspell-mode-map
        ("C-c $" . flyspell-correct-at-point))

  :custom
  (flyspell-correct-highlight t "Highlight word being corrected."))

(use-package which-key
  :diminish
  which-key-mode

  :commands
  which-key-mode

  :hook
  ((prog-mode text-mode special-mode) . which-key-mode)

  :custom
  (which-key-popup-type 'minibuffer "Use the minibuffer for the key display.")
  (which-key-sort-order 'which-key-key-order "Use the default sort order.")
  (which-key-idle-delay 2.0 "How long to wait before offering a guide.")
  (which-key-max-description-length 27 "Truncate descriptions.")
  (which-key-add-column-padding 0 "Left padding for key display.")
  (which-key-show-prefix 'bottom "Display currently typed prefix at bottom."))

(use-package transpose-frame
  :commands
  transpose-frame
  flip-frame
  flop-frame
  rotate-frame
  rotate-frame-clockwise
  rotate-frame-anticlockwise

  :bind
  (:map global-map
        ("C-c <up>" . transpose-frame)
        ("C-c <down>" . rotate-frame-clockwise)))

(use-package rainbow-delimiters
  :diminish
  rainbow-delimiters-mode

  :commands
  rainbow-delimiters-mode

  :hook
  ((prog-mode) . rainbow-delimiters-mode))

(use-package cmake-mode
  :commands
  cmake-mode

  :mode
  (("\\(CMakeLists\\.txt\\|\\.cmake\\)\\'" . cmake-mode)))

(use-package yaml-mode
  :commands
  yaml-mode

  :mode
  (("\\(\\.[Yy][Mm][Ll]\\|\\.yaml\\)\\'" . yaml-mode)))

(use-package csv-mode
  :commands
  csv-mode

  :mode
  (("\\.[Cc][Ss][Vv]\\'" . csv-mode)))

(use-package lua-mode
  :commands
  lua-mode

  :mode
  (("\\.lua\\'" . lua-mode))

  :custom
  (lua-indent-level 2 "We prefer to indent in Lua to 2 spaces."))

(use-package markdown-mode
  :commands
  markdown-mode
  gfm-mode

  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\(\\.md\\|\\.markdown\\)\\'" . markdown-mode)))

;; Local Variables:
;; mode: emacs-lisp
;; coding: utf-8-unix
;; fill-column: 80
;; require-final-newline: t
;; End:

;;; core-config.el ends here
