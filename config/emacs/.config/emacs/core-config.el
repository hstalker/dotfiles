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

;; Internal/built-in package configuration
;; Configure things defined in the core-most Emacs C code.
;; If you aren't sure where something goes, prefer to put it here instead of in
;; global scope.
(use-package emacs
  :no-require t ; Don't load `emacs.el' (obviously - it won't work)

  :init
  (defun hgs--new-frame-setup (&optional frame)
    "Configure the given frame. Should be attached to
`after-make-frame-functions' hook."
    (unless frame
      (setq frame (selected-frame)))
    (unless (daemonp)
      (with-selected-frame frame
        ;; Setup block for all Emacs instances with frame
        ;; Set a default font
        (cl-flet ((get-font-family (lambda (font) (car font)))
                  (get-font-size (lambda (font) (cadr font))))
          (let ((latin-font '("7x13" 12))
                (unicode-font '("DejaVu Sans" 12)))
            (set-face-attribute 'default nil
                                :family (get-font-family latin-font))
            (set-fontset-font t 'unicode
                              (font-spec :family (get-font-family unicode-font)
                                         :size (get-font-size unicode-font))
                              nil)
            (set-fontset-font t 'latin
                              (font-spec :family (get-font-family latin-font)
                                         :size (get-font-size latin-font))
                              nil 'prepend)))
        (if (display-graphic-p frame)
            (progn
              ;; Setup block for GUI Emacs
              (modify-frame-parameters frame
                                       '((fullscreen . nil)
                                         (vertical-scroll-bar . nil)
                                         (horizontal-scroll-bar . nil))))
          (progn
            ;; Setup block for terminal Emacs
            )))))
  ;; For non daemon run, as init file runs after the initial frame is created
  (hgs--new-frame-setup)
  ;; Add hook for any future frames
  (add-hook 'after-make-frame-functions #'hgs--new-frame-setup)
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1)
  (menu-bar-mode -1)
  (tool-bar-mode -1)

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
   ;; mutliplexing.

   ;; General Structure:
   ;; (REGULAR-EXPRESSION-STRING
   ;;  ORDERED-LIST-OF-WINDOW-SELECTION-FUNCTIONS
   ;;  OPTIONS...)
   '(("^\\*\\([Hh]elp\\|[Cc]ustom\\|[Ff]aces\\).*"
      (display-buffer-reuse-mode-window
       display-buffer-in-side-window)
      (window-width . 0.25)
      (side . right)
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

     ;; Helm uses windows rather than the mini-buffer, so we need a workaround
     ;; to prevent the `display-buffer-base-action' causing things like
     ;; `helm-M-x' to take up the entire window. TO be honest, it's
     ;; probably preferable to control Helm windowing through this rather than
     ;; Helm itself, since it's centralized, but we're going to avoid touching
     ;; any additional configuration and letting Helm do its thing.
     ("^\\*[Hh]elm.*"
      (display-buffer-reuse-mode-window
       display-buffer-at-bottom)))
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
   ((text-mode) . #'turn-on-auto-fill)))

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

  :functions
  whitespace-mode
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
  ((prog-mode text-mode) . #'hgs--reset-whitespace-mode-local-hack)

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
        ("=" . #'ediff-revision))

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
         ("C-x d" . #'dired))
   (:map dired-mode-map
         ("^" . #'hgs--dired-up-directory-clean)))

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

  :functions
  winner-mode

  :config
  ;; Enable winnder mdoe for undo/redo of window layout changes
  (winner-mode t))

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
           ("l" . #'org-store-link)
           ("c" . #'org-capture)
           ("a" . #'org-agenda)
           ("j" . #'org-clock-goto))

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
  :functions
  clang-format+-mode

  :init
  (use-package clang-format)

  :hook
  ((c-mode- c++-mode objc-mode) . #'clang-format+-mode)

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
  :functions
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
  :functions
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
  (projectile-completion-system 'helm "Use Helm as the completion backend.")
  (projectile-project-search-path
   `(,hgs-project-directory ,hgs-user-directory)
   "Where shoudl projectile search?")
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


(use-package helm
  ;; We always want to load this if we have it in load-path.
  :demand t

  :diminish
  helm-mode
  helm-autoresize-mode

  :functions
  helm-mode
  helm-autoresize-mode

  :config
  (require 'helm-config)
  (helm-mode t)
  (helm-autoresize-mode t)

  ;; We explicitly don't want to set these up as autoloads, as that would mean
  ;; starting emacs when unable to load helm leaves us helpless.
  (bind-keys :map global-map
             ([remap execute-extended-command] . helm-M-x)
             ([remap switch-to-buffer] . helm-mini)
             ([remap bookmark-jump] . helm-filtered-bookmarks)
             ([remap find-file] . helm-find-files)
             ([remap yank-pop] . helm-show-kill-ring)
             ([remap apropos-command] . helm-apropos)
             ([remap jump-to-register] . helm-register)
             ([remap dabbrev-expand] . helm-dabbrev))


  :bind-keymap
  ("C-c h" . helm-map)

  :custom
  (helm-lisp-fuzzy-completion t "Enable Lisp fuzzy completion.")
  (helm-apropos-fuzzy-match t "Enable fuzzy matching in apropos.")
  (helm-locate-fuzzy-match t "Enable fuzzy matching in locate.")
  (helm-imenu-fuzzy-match t "Enable fuzzy matching in imenu listings.")
  (helm-buffers-fuzzy-matching t "Enable fuzzy matching for buffer search.")
  (helm-recentf-fuzzy-match t "Enable fuzzy matching for recentf search.")
  (helm-google-suggest-use-curl-p
   (not (not (executable-find "curl")))
   "use Curl to fetch web data when available.")
  (helm-scroll-amount 8 "Amount of lines to scroll in other window.")
  (helm-ff-search-library-in-sexp
   t
   "Try to find the library inside a sexp at point.")
  (helm-move-to-line-cycle-in-source t "Cycle lists when reaching extents.")
  (helm-split-window-inside-p
   t
   "Force Helm to open a split inside current window.")
  (helm-autoresize-min-height 25 "Minimum height for the Helm window as %.")
  (helm-autoresize-max-height 50 "Minimum height for the Helm window as %.")
  (helm-grep-file-path-style
   'relative
   "Show the relative path of the file in results. Extremely useful in a
project with deeply nested and repetitive structure."))

(use-package helm-projectile
  :after
  helm
  projectile

  :functions
  helm-projectile-grep

  :bind
  (:map projectile-command-map
        ("s g" . #'helm-projectile-grep)))

(use-package helm-descbinds
  :after helm

  :diminish
  helm-descbinds-mode

  :hook
  ((prog-mode text-mode) . helm-descbinds-mode))

(use-package helm-swoop
  :after helm

  :commands
  helm-swoop

  :bind
  ((:map helm-command-prefix
         ("M-s s" . #'helm-swoop))
   (:map isearch-mode-map
         ("M-s s" . #'helm-swoop)))

  :custom
  (helm-multi-swoop-edit-save
   t
   "Save the buffer when a swoop edit completes.")
  (helm-swoop-split-with-multiple-windows
   t
   "Split window inside the current window.")
  (helm-swoop-split-direction
   'split-window-vertically
   "Split vertically not horizontally.")
  (helm-swoop-speed-or-color t "We prefer pretty colors over speed.")
  (helm-swoop-move-to-line-cycle t "Cycle inside lines (beginning/end).")
  (helm-swoop-use-line-number-face t "Use optional face for line numbers.")
  (helm-swoop-use-fuzzy-match t "Enable fuzzy matching for helm swoop."))

(use-package solarized-theme
  :config
  (load-theme 'solarized-light 'no-confirm)
  ;; Disable background theming if in terminal as this causes breakage.
  ;; This means the terminal itself must be appropriately themed.
  ;; Be careful about not doing this if we are in daemon mode though, since
  ;; it'll break GUI emacsclient (invalid argument stringp errors).
  (if (not (or (daemonp) (display-graphic-p)))
      (add-to-list 'default-frame-alist '(background-color . nil)))

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
        ("C-x g" . #'magit-status)
        ("C-x M-g" . #'magit-dispatch)))

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
        ("C-=" . #'er/expand-region)))

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
   '((company-files
      company-keywords
      company-capf
      company-yasnippet
      company-dabbrev-code
      company-abbrev))
   "Set default company backends to use.")
  (company-idle-delay 0.5 "How long to wait before offering completion.")
  (company-show-numbers t "Number the completion options.")
  (company-tooltip-limit 15 "Cap the number of candidates on display.")
  (company-minimum-prefix-length
   2
   "Minimum length of prefix before completion.")
  (company-tooltip-align-annotations t "Align candidates to the right.")
  (company-tooltip-flip-when-above
   nil
   "Don't invert navigation direction when near the bottom of the page."))

(use-package undo-tree
  :diminish
  global-undo-tree-mode
  undo-tree-mode

  :hook
  ((prog-mod text-mode) . undo-tree-mode))

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

(use-package which-key
  :diminish
  which-key-mode

  :hook
  ((prog-mode text-mode) . which-key-mode)

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
        ("C-c <up>" . #'transpose-frame)
        ("C-c <down>" . #'rotate-frame-clockwise)))

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
