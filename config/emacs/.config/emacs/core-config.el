;;; core-config.el --- Core emacs configurations -*- lexical-binding: t; -*-

;;; Commentary:

;; Performs base-line Emacs and package configuration upon which the end-user
;; can build.

;;; Code:

(require 'cl-lib)
(require 'files)
(require 'custom)
(require 'minmacs)
(require 'use-package)

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
  "Byte (re)compile our configuration scripts."
  (interactive)
  (dolist (file (directory-files-recursively hgs-config-directory ".*.el$"))
    ;; Skip the lock files
    (unless (member (file-name-sans-extension (file-name-base file))
                    '("core-lock" "custom-lock"))
      (byte-recompile-file file nil 0))))

(defun hgs-reload-configuration ()
  "Reload our Emacs configuration."
  (interactive)
  (message "Reloading user configuration from %s..." user-init-file)
  (load user-init-file))

(defun hgs-copy-file-path ()
  "Copy the currently visited file's path to clipboard."
  (interactive)
  (let ((file-path (if (equal major-mode 'dired-mode)
                       default-directory
                     (buffer-file-name))))
    (when file-path
      (kill-new file-path)
      (message "Copied visited file path '%s' to the clipboard." file-path))))

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

  (defun hgs--setup-default-tui (_frame)
    "Set default terminal UI for the given frame."
    (menu-bar-mode -1))

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
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))

  ;; Better support handling processes with large amounts of data like LSP
  (setq read-process-ouput-max (* 1024 1024))

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
        (concat hgs-cache-directory "games/shared"))

  ;; Enable narrow-to-* family of functions, as they're disabled by default
  (put 'narrow-to-region 'disabled nil)
  (put 'narrow-to-defun 'disabled nil)
  (put 'narrow-to-page 'disabled nil)

  (bind-keys :map global-map
             ([remap suspend-frame] . hgs--suspend-frame))

  ;; Indicate the depth of recursion of the mini-buffer in the mode-line
  (minibuffer-depth-indicate-mode +1)

  :custom
  (require-final-newline t "We should always require a final newline.")
  (case-fold-search t "Should have non-case-sensitive search by default.")
  (use-dialog-box nil "All dialog boxes should instead be mini-buffer prompts.")
  (kill-whole-line nil "C-k shouldn't kill entire line.")
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
  (auto-save-list-file-prefix (concat hgs-cache-directory "auto-save-list")
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
        (concat hgs-cache-directory "games/gamegrid")))

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
  :config
  ;; Attempt to undo the window configuration change when exiting an Ediff
  ;; session.
  (add-hook 'ediff-after-quit-hook-internal #'winner-undo)

  :custom
  (ediff-keep-variants nil "Kill Ediff buffers on exit by default.")
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

;; Active regions should be deleted by normal deletion commands like expected
(use-package delsel
  :demand t

  :config
  (delete-selection-mode))

;; Enable highlighting of current line if we have more recent fast line
;; highlighting (>=27).
(unless (version< emacs-version "27")
  (use-package hl-line
    :commands
    hl-line-mode

    :hook
    ((prog-mode text-mode) . hl-line-mode)))

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

(when (version<= "27.1" emacs-version)
  (use-package so-long
    :demand t

    :commands
    global-so-long-mode
    so-long-minor-mode
    so-long

    :config
    ;; so-long will automatically figure out when to activate with the global
    ;; mode on.
    (global-so-long-mode +1)

    :custom
    (so-long-action #'so-long-minor-mode "Action to perform when
long lines are detected.")
    (so-long-threshold 1000 "Threshold for what length is considered long.")
    (so-long-max-lines 100 "Number of non-blank lines to test for length.")))

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
   `((".*" ,(concat hgs-cache-directory) "backup")
     t)
   "Put autosaves in the cache directory.")
  (backup-directory-alist
   `((".*" . ,(concat hgs-cache-directory "backup")))
   "Put backups in the cache directory.")
  (backup-by-copying t "Always copy rather than symlink for backups."))

(use-package subword
  :diminish
  global-subword-mode
  subword-mode

  :hook
  ((prog-mode text-mode special-mode) . subword-mode))

(use-package recentf
  :diminish
  recentf

  :hook
  ((prog-mode text-mode special-mode) . recentf-mode)

  :custom
  (recentf-save-file (concat hgs-cache-directory "recentf")
                     "Place the recentf cache into our cache directory.")
  (recentf-max-menu-items 25 "Maximum number of menu items to show.")
  (recentf-max-saved-items 25 "Maximum number of items to save."))

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
  (setq desktop-dirname (concat hgs-data-directory "desktop"))

  :custom
  (desktop-base-file-name "autosave" "Name of the desktop save file.")
  (desktop-base-lock-name "autosave-lock" "Name of the lock for desktop
package."))

(use-package tramp
  :custom
  (tramp-auto-save-directory
   (concat hgs-cache-directory "tramp-auto-save")
   "Directory to place auto-saves when editing via Tramp.")
  (tramp-backup-directory-alist
   backup-directory-alist
   "Put Tramp backups in the same place as local backups.")
  (tramp-persistency-file-name
   (concat hgs-cache-directory "tramp-persistency.el")
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
  (eshell-directory-name (concat hgs-data-directory "eshell")
                         "Use cache directory for storing files (e.g. aliases,
history etc.)")
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

(use-package url
  :custom
  (url-cache-directory
   (concat hgs-cache-directory "url")
   "Put the url package's cache directory where we would expect.")
  (url-configuration-directory
   (concat hgs-data-directory "url")
   "Put the url package's configuration directory in the data directory."))

(use-package bookmark
  :custom
  (bookmark-default-file
   (concat hgs-data-directory "bookmarks")
   "Put bookmarks in the data directory."))

(use-package custom
  :custom
  (custom-theme-directory
   (concat hgs-config-directory "themes")
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

  :init
  (which-key-add-key-based-replacements
    "C-c o" "Org")

  :custom
  (org-directory hgs-org-directory
                 "Base path to store org files inside by default.")
  (org-default-notes-file
   (concat hgs-org-directory "notes.org")
   "Put org notes into the appropriate file & directory by default.")
  (org-agenda-files
   `(,hgs-org-directory)
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
         (concat org-directory "todo.org")))
      "* TODO %? %^G\n%U" :empty-lines 1)
     ("s" "Scheduled Todo" entry
      (file
       (lambda ()
         (concat org-directory "todo.org")))
      "* TODO %? %^G\nSCHEDULED: %^t\n%U" :empty-lines 1)
     ("d" "Deadline" entry
      (file
       (lambda ()
         (concat org-directory "todo.org")))
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

(use-package dockerfile-mode
  :commands
  dockerfile-mode

  :mode
  (("Dockerfile\\'" . dockerfile-mode))

  :config
  ;; Allow the use to put a file-local variable specifying the image name.
  (put 'docker-image-name 'safe-local-variable #'stringp))

(use-package protobuf-mode
  :commands
  protobuf-mode

  :mode
  (("\\.\\(pb\\|proto\\)\\'" . protobuf-mode)))

(use-package bazel
  :commands
  bazel-build-mode
  bazel-workspace-mode
  bazelrc-mode
  bazel-starlark-mode

  :mode
  (("/BUILD\\(\\..*\\)?\\'" . bazel-build-mode)
   ("/WORKSPACE\\'" . bazel-workspace-mode)
   ("\\.bzl\\'" . bazel-starlark-mode)
   ("\\.bazelrc\\'" . bazelrc-mode)))

(use-package meson-mode
  :commands
  meson-mode

  :mode
  (("\\.meson\\'" . meson-mode)
   ("/meson\\.build\\'" . meson-mode)))

(use-package jq-mode
  :after
  json-mode

  :defines
  json-mode-map

  :commands
  jq-mode
  jq-interactively

  :mode
  ("\\.jq\\'" . jq-mode)

  :bind
  (:map json-mode-map
        ("C-c C-j" . jq-interactively)))

(use-package toml-mode
  :commands
  toml-mode

  :mode
  (("\\.toml\\'" . toml-mode)))

;; Dim non-focused windows for clarity. This seems to cause massive terminal
;; slow-down when changing active window, so I might eventually remove it.
(use-package dimmer
  :demand t

  :diminish
  dimmer-mode

  :init
  ;; Apply some fixes when/if these packages load to prevent dimmer from
  ;; interfering with their visibility.
  (defmacro hgs--apply-dimmer-fix (package-name)
    (let ((package-name-str (symbol-name package-name)))
    `(with-eval-after-load ,package-name-str
       (funcall (intern (format "dimmer-configure-%s" ,package-name-str))))))

  :config
  (dolist (_pkg '(magit which-key hydra org posframe gnus helm company-box))
    (hgs--apply-dimmer-fix _pkg))

  (dimmer-mode +1)

  :custom
  (dimmer-adjustment-mode :both "Dim the other windows' fore/backgrounds.")
  (dimmer-fraction 0.15 "Higher means greater dimming.")
  (dimmer-watch-frame-focus-events
   nil
   "Don't react to frame-wide focusing changes. Needed to avoid flashing on
mouse navigation."))

(use-package avy
  :defines
  avy-order-closest

  :commands
  avy-goto-char
  avy-goto-char-2
  avy-goto-char-timer
  avy-isearch
  avy-goto-word
  avy-goto-line

  :bind
  (:map global-map
        ("C-'" . avy-goto-char))
  (:map isearch-mode-map
        ("C-'". avy-isearch))

  :custom
  (avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l) "Use home row for Avy prompts.")
  (avy-keys-alist '() "Alist of Avy commands to keys to use for prompts. Falls
back to `avy-keys'")
  (avy-style 'at "Overlay display style.")
  (avy-styles-alist '() "Alist of Avy commands to overlay display styles.")
  (avy-background nil "Use a gray background during selection.")
  (avy-all-windows t "Scan all windows on the selected for selection.")
  (avy-case-fold-search t "Ignore case for search.")
  (avy-highlight-first nil "Don't highlight the first decision character, only
first non-terminating decision characters.")
  (avy-timeout-seconds 0.5 "How long `*-timer' commands should wait.")
  (avy-orders-alist
   '((avy-goto-char . avy-order-closest))
   "Alist allowing for specifying commands to use fewer characters when closer
to point."))

(use-package page-break-lines
  :diminish
  page-break-lines-mode

  :commands
  page-break-lines-mode)

(use-package all-the-icons
  :after
  memoize

  :config
  ;; Try to automatically install the fonts if they appear missing
  (when (and (not (member "all-the-icons" (font-family-list)))
             (not (daemonp))
             (window-system))
    (all-the-icons-install-fonts t)))

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

(use-package selectrum
  :demand t

  :diminish
  selectrum-mode

  :config
  (selectrum-mode +1))


(use-package prescient
  :after
  selectrum

  :demand t

  :diminish
  prescient-persist-mode

  :commands
  prescient-persist-mode

  :config
  (prescient-persist-mode +1)

  :custom
  (prescient-history-length
   100
   "Number of recently selected candidates to show at the top of the list.")
  (prescient-frequency-decay
   0.997
   "How much to decrease candidates' priorities for subsequent non-selections.")
  (prescient-save-file
   (concat hgs-cache-directory "prescient-statistics")
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

(use-package company
  :hook
  (prog-mode . company-mode)
  ((org-mode text-mode) . company-mode)
  (prog-mode . hgs--set-prog-mode-company-backends)
  ((org-mode text-mode) . hgs--set-text-mode-company-backends)

  :init
  (defun hgs--set-prog-mode-company-backends ()
    "Set appropriate Company back-ends for programming mode buffers."
    (make-local-variable 'company-backends)
    (setq-local company-backends
                '((company-capf
                   company-yasnippet
                   company-keywords
                   company-files)
                  (company-etags
                   company-dabbrev-code)
                  (company-dabbrev))))

  (defun hgs--set-text-mode-company-backends ()
    "Set appropriate Company back-ends for text mode buffers."
    (make-local-variable 'company-backends)
    (setq-local company-backends
                '((company-yasnippet
                   company-ispell
                   company-keywords
                   company-files)
                  (company-dabbrev))))

  :custom
  (company-backends
   '((company-capf
      company-yasnippet
      company-keywords
      company-files)
     (company-dabbrev-code
      company-etags)
     (company-ispell)
     (company-abbrev))
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

(use-package consult
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
        ([remap bookmark-jump] . consult-bookmark)
        ("C-x M-:" . consult-complex-command)
        ("C-x b" . consult-buffer)
        ("C-x 4 b" . consult-buffer-other-window)
        ("C-x 5 b" . consult-buffer-other-frame))
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
  (consult-narrow-key ">" "Specify the key used for explicit narrowing.")
  (consult-widen-key "<" "Specify key used for explicit widening.")
  (consult-preview-key 'any "Trigger Consult previews with any key press.")
  (consult-project-root-function
   #'projectile-project-root
   "Use Projectile for finding the project root."))

(use-package marginalia
  :after
  selectrum

  :demand t

  :commands
  marginalia-mode

  :diminish
  marginalia-mode

  :bind
  (:map global-map
        ("M-A" . marginalia-cycle))
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle))

  :config
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
        ("C-S-b" . embark-become))
  (:map embark-file-map
        ("j" . dired-jump))

  :init
  (which-key-add-key-based-replacements
    "C-c e" "Embark")

  :custom
  (embark-quit-after-action
   nil
   "Don't leave the minibuffer etc. after acting.")
  (embark-collect-initial-view-alist
   '((file . list)
     (buffer . list)
     (symbol . list)
     (line . list)
     (xref-location . list)
     (kill-ring . zebra)
     (t . list))
   "Setup default view per item category.")
  (embark-collect-live-update-delay
   0.5
   "Delay between input and collect update.")
  (embark-collect-initial-delay
   0.8
   "Delay before popping up collection."))

(use-package flyspell-correct
  :after
  flyspell

  :bind
  (:map flyspell-mode-map
        ("C-c $" . flyspell-correct-at-point))

  :custom
  (flyspell-correct-highlight t "Highlight word being corrected."))

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
        (concat hgs-data-directory "async-bytecomp.log"))

  :hook
  ((after-init . async-bytecomp-package-mode)
   (dired-mode . dired-async-mode)))

(use-package transient
  :custom
  (transient-levels-file
   (concat hgs-cache-directory "transient/levels.el")
   "Where to place the Transient levels cache file.")
  (transient-values-file
   (concat hgs-cache-directory "transient/values.el")
   "Where to place the Transient values cache file.")
  (transient-history-file
   (concat hgs-cache-directory "transient/history.el")
   "Where to place the Transient history cache file."))

(use-package with-editor
  :init
  (defun hgs--with-editor-export-editor ()
    "Run `with-editor-export-editor' once for each possible editor variable."
    (dolist (var '("EDITOR"
                   "VISUAL"
                   "GIT_EDITOR"
                   "HG_EDITOR"))
      (with-editor-export-editor var)))

  :commands
  with-editor-export-editor
  with-editor-async-shell-command
  with-editor-shell-command

  :hook
  ((shell-mode term-exec eshell-mode vterm-mode)
   . hgs--with-editor-export-editor)

  :bind
  (:map global-map
        ([remap async-shell-command] . with-editor-async-shell-command)
        ([remap shell-command] . with-editor-shell-command)))

;; Trim whitespace on touched lines only automatically when saving
(use-package ws-butler
  :diminish
  ws-butler-global-mode
  ws-butler-mode

  :commands
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

  :bind-keymap
  ("C-c &" . yas-keymap)

  :init
  (which-key-add-key-based-replacements
    "C-c &" "Yasnippet")

  :config
  ;; Needed to force Emacs to load up all snippets given in our personal
  ;; snippet directories
  (yas-reload-all)

  :custom
  (yas-snippet-dirs
   `(,(concat hgs-config-directory "snippets"))
   "Where to find snippet definitions."))

(use-package yasnippet-snippets
  :after yasnippet

  :config
  (yasnippet-snippets-initialize))

(use-package which-key
  :diminish
  which-key-mode

  :commands
  which-key-mode
  which-key-add-key-based-replacements
  which-key-add-major-mode-key-based-replacements

  :hook
  ((prog-mode text-mode special-mode) . which-key-mode)

  :config
  (which-key-setup-side-window-bottom)

  :custom
  (which-key-popup-type 'side-window "Use the minibuffer for the key display.")
  (which-key-side-window-slot 0 "Slot of the side window to use.")
  (which-key-side-window-location 'bottom "Place on the bottom.")
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

(use-package restclient
  :commands
  restclient-mode

  :mode
  (("\\.restclient\\'" . restclient-mode)))

(use-package string-inflection
  :commands
  string-inflection-get-current-word
  string-inflection-upcase
  string-inflection-upcase-function
  string-inflection-upcase-p
  string-inflection-capital-underscore
  string-inflection-capital-underscore-function
  string-inflection-capital-underscore-p
  string-inflection-underscore
  string-inflection-underscore-function
  string-inflection-underscore-p
  string-inflection-camelcase
  string-inflection-camelcase-function
  string-inflection-camelcase-p
  string-inflection-pascal-case
  string-inflection-pascal-case-function
  string-inflection-pascal-case-p
  string-inflection-kebab-case
  string-inflection-kebab-case-function
  string-inflection-kebab-case-p

  :init
  (defun hgs-restyle-dwim ()
    "Completing read enabled restyling of the specified region or current word.
Allows for converting the given region between kebab-case,
snake_case, Snake_Case, camelCase, PascalCase, and UPPER_CASE."
    (interactive)
    ;; We don't want to move the point or the mark
    (save-mark-and-excursion
      (let* ((selection (string-inflection-get-current-word))
             (choices '(("UPPER_CASE" .
                         string-inflection-upcase-function)
                        ("snake_case" .
                         string-inflection-underscore-function)
                        ("Snake_Case" .
                         string-inflection-capital-underscore-function)
                        ("PascalCase" .
                         string-inflection-pascal-case-function)
                        ("camelCase" .
                         string-inflection-camelcase-function)
                        ("kebab-case" .
                         string-inflection-kebab-case-function)))
             (choice (completing-read "Select target style: "
                                      choices
                                      nil ; Predicate?
                                      'require-match))
             (action (alist-get choice choices nil nil 'string-equal)))
        ;; Replace region with result of restyle
        (insert (funcall action selection))))))

(use-package editorconfig
  :diminish
  editorconfig-mode

  :commands
  editorconfig-mode

  :hook
  ((prog-mode) . editorconfig-mode))

(use-package wgrep
  :commands
  wgrep-mode

  :bind
  (:map grep-mode-map
        ("C-c C-p" . wgrep-mode))

  :custom
  (wgrep-auto-save-buffer
   nil
   "Don't save buffer automatically when `wgrep-finish-edit'.")
  ;; (wgrep-enable-key "\C-c\C-p" "Key to make the grep buffer `wgrep' editable.")
  (wgrep-change-readonly-file
   nil
   "Don't apply changes regardless of whether the buffer is read-only."))

(unless (null module-file-suffix)
  (use-package vterm))

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

(use-package dashboard
  :demand t

  :diminish
  dashboard-mode
  page-break-lines-mode

  :config
  (dashboard-setup-startup-hook)

  :custom
  (dashboard-banner-logo-title "Welcome to Emacs!" "The title message.")
  (dashboard-startup-banner
   ;; Straight doesn't seem to like grabbing the banner from here
   (concat hgs-config-directory "data/banner.txt")
   "Can be nil, 'official for the Emacs logo, 1, 2 or 3 for the
text banners, or a path to an image or text file.")
  (dashboard-center-content t "Center the dashboard content.")
  (dashboard-show-shortcuts t "Show the per section jump indicators.")
  (dashboard-set-heading-icons t "Show widget heading icons.")
  (dashboard-set-file-icons t "Show file icons.")
  (dashboard-items
   '((recents . 5)
     (bookmarks . 5)
     (projects . 5)
     (agenda . 5)
     (registers . 5))
   "Show these widgets in format `(SELECTOR-SYMBOL . ENTRY-COUNT)'.")
  (dashboard-set-navigator t "Show the navigator below the banner.")
  (dashboard-set-init-info t "Show packages loaded and initialization time.")
  (dashboard-set-footer t "Enable the randomly selected footnote.")
  (dashboard-projects-switch-function
   #'projectile-switch-project-by-name
   "Which function to use for switching projects from the dashboard.")
  (dashboard-week-agenda t "Show upcoming seven days' agenda.")
  (initial-buffer-choice
   (lambda ()
     (get-buffer "*dashboard*"))
   "Show the dashboard as the initial buffer even for the Emacs client."))

(use-package clang-format)

(use-package clang-format+
  :after
  clang-format

  :commands
  clang-format+-mode

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

(use-package hydra)

(use-package undo-tree
  :diminish
  global-undo-tree-mode
  undo-tree-mode-hook

  :commands
  global-undo-tree-mode
  undo-tree-mode
  undo-tree-undo
  undo-tree-redo
  undo-tree-visualize

  :hook
  ((prog-mode text-mode) . undo-tree-mode)

  :custom
  (undo-tree-history-directory-alist
   `(("." . ,(concat hgs-cache-directory "undo-tree")))
   "Put history backups in the cache directory."))

(use-package projectile
  :commands
  projectile-mode
  projectile-switch-project-by-name
  projectile-project-root

  :hook
  ((prog-mode text-mode special-mode) . projectile-mode)

  :bind-keymap
  ("C-c p" . projectile-command-map)

  :init
  (which-key-add-key-based-replacements
    "C-c p" "Projectile")

  :config
  ;; Make setting the projectile build-related variables via local variables
  ;; safe.
  (put 'projectile-project-configure-cmd 'safe-local-variable #'stringp)
  (put 'projectile-project-compilation-cmd 'safe-local-variable #'stringp)
  (put 'projectile-project-test-cmd 'safe-local-variable #'stringp)
  (put 'projectile-project-run-cmd 'safe-local-variable #'stringp)

  :custom
  (projectile-completion-system
   'default
   "Use Selectrum (or rather: `completing-read') as the completion backend.")
  (projectile-project-search-path
   `(,hgs-project-directory ,hgs-user-directory)
   "Where should projectile search?")
  (projectile-known-projects-file
   (concat hgs-cache-directory "projectile-bookmarks.eld")
   "Where to cache known projects.")
  (projectile-use-git-grep nil "Don't use git-grep over other tools.")
  (projectile-cache-file
   (concat hgs-cache-directory "projectile.cache")
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

(use-package selectrum-prescient
  :after
  selectrum
  prescient

  :demand t

  :diminish
  selectrum-prescient-mode

  :commands
  selectrum-prescient-mode

  :config
  (selectrum-prescient-mode +1)

  :custom
  (selectrum-prescient-enable-filtering t "Enable filtering for selectrum.")
  (selectrum-prescient-enable-sorting t "Enable sorting for selectrum."))

(use-package company-prescient
  :after
  company
  prescient

  :demand t

  :diminish
  company-prescient-mode

  :commands
  company-prescient-mode

  :config
  (company-prescient-mode +1)

  :custom
  (company-prescient-sort-length-enable nil "Don't resort company's already
partially sorted lists by length, as this ruins the sort order."))

(use-package embark-consult
  :after
  embark
  consult

  ;; This package is needed by default to integrate embark actions with consult
  ;; commands properly (for example, consult-buffer's items won't have
  ;; appropriate actions without it). The hook/command prevents loading this
  ;; immediately, so we must demand it.
  :demand t

  :commands
  embark-consult-preview-minor-mode

  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))

(use-package json-mode
  :commands
  json-mode

  :mode
  (("\\.json\\'" . json-mode)))

(use-package flycheck
  :commands
  flycheck-mode

  :bind-keymap
  ("C-c !" . flycheck-keymap-prefix)

  :hook
  ((prog-mode) . flycheck-mode)

  :init
  (which-key-add-key-based-replacements
    "C-c !" "Flycheck"))

(use-package magit
  :after
  transient
  with-editor

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

(use-package consult-flycheck
  :after
  consult
  flycheck

  :commands
  consult-flycheck

  :bind
  (:map flycheck-command-map
        ("!" . consult-flycheck)))

(use-package lsp-mode
  :after
  which-key

  :commands
  lsp
  lsp-deferred
  lsp-install-server

  :defines
  lsp-mode-map

  :mode
  ("\\.vim\\(rc\\)?\\'" . lsp-deferred)

  :hook
  ((c++-modec-mode objc-mode) . lsp-deferred)
  (python-mode . lsp-deferred)
  ((html-mode css-mode xml-mode) . lsp-deferred)
  ((yaml-mode json-mode) . lsp-deferred)
  (sh-mode . lsp-deferred)
  ;; For which-key integration
  (lsp-mode . lsp-enable-which-key-integration)
  (lsp-mode . lsp-ui-mode)

  :custom
  (lsp-keymap-prefix "C-c l" "Prefix key-binding for LSP mappings.")
  (lsp-server-install-dir
   (concat hgs-data-directory "lsp")
   "Directory in which to install automatically installed LSP servers.")
  (lsp-session-file
   (concat hgs-cache-directory "lsp-session-v1")
   "Place the LSP session information in the cache."))

(use-package lsp-ui
  :after
  lsp-mode

  :commands
  lsp-ui-mode

  :custom
  (lsp-ui-sideline-enable t "Turn on the sideline.")
  (lsp-ui-sideline-show-diagnostics t "Show diagnostics in the sideline.")
  (lsp-ui-sideline-show-hover nil "Don't show hover messages in the sideline.")
  (lsp-ui-sideline-show-code-actions t "Show code actions in the sideline.")
  (lsp-ui-sideline-update-mode
   'point
   "'line to update when changing line, 'point to update when changing point.")
  (lsp-ui-sideline-delay 0.2)

  (lsp-peek-enable nil "Don't use peek. Prefer to use the narrowing framework.")

  (lsp-ui-doc-enable t "Show documentation in frame.")
  (lsp-ui-doc-position 'top "Where to display the documentation.
'top, 'bottom or 'at-point.")
  (lsp-ui-doc-delay 0.2 "Seconds delay before showing the documentation.")
  (lsp-ui-doc-show-with-cursor
   nil
   "When non-nil, move the cursor over a symbol to show the documentation.")
  (lsp-ui-doc-show-with-mouse
   nil
   "When non-nil, show documentation when hovering over a releveant symbol with
the mouse."))

(use-package consult-lsp
  :after
  lsp-mode

  :commands
  consult-lsp-diagnostics
  consult-lsp-symbols

  :bind
  (:map lsp-mode-map
        ([remap xref-find-apropos] . consult-lsp-symbols)))

(use-package treemacs
  :commands
  treemacs
  treemacs-find-file
  treemacs-find-tag
  treemacs-select-window
  treemacs-delete-other-windows
  treemacs-bookmark
  treemacs-filewatch-mode
  treemacs-follow-mode
  treemacs-git-mode
  treemacs-fringe-indicator-mode

  :bind
  (:prefix "C-c t"
           :prefix-map hgs--treemacs-prefix-map
           :prefix-docstring "Treemacs commands"
           ("t" . treemacs)
           ("g" . treemacs-select-window)
           ("1" . treemacs-delete-other-windows)
           ("B" . treemacs-bookmark)
           ("f" . treemacs-find-file)
           ("T" . treemacs-find-tag))

  :hook
  ((treemacs-mode) . treemacs-follow-mode)
  ((treemacs-mode) . treemacs-filewatch-mode)
  ((treemacs-mode) . hgs--treemacs-fringe-indicator-mode)
  ((treemacs-mode) . hgs--treemacs-git-mode)

  :init
  (defun hgs--treemacs-fringe-indicator-mode ()
    (treemacs-fringe-indicator-mode 'always))
  (defun hgs--treemacs-git-mode ()
    (unless (null (executable-find "git"))
      (if (null treemacs-python-executable)
          (treemacs-git-mode 'simple)
        (treemacs-git-mode 'deferred))))

  ;; Force treemacs toggling to *not* focus the treemacs window
  (define-advice treemacs
      (:around (orig-fn &rest args) treemacs-no-select)
    (save-selected-window
      (apply orig-fn args)))

  (which-key-add-key-based-replacements
    "C-c t" "Treemacs")

  :config
  ;;(treemacs-resize-icons 44) ; Doubles icon size for Hi-DPI

  :custom
  (treemacs-persist-file
   (concat hgs-cache-directory "treemacs-persist")
   "Place Treemacs persistence file in the cache.")
  (treemacs-sorting 'alphabetic-asc "Sort entries by alphabetical ordering.")
  (treemacs-position 'left "Put the Treemacs window on the left.")
  (treemacs-display-in-side-window
   t
   "Put the Treemacs window in a side window.")
  (treemacs-width 35 "Width of the Treemacs window as a percentage.")
  (treemacs-is-never-other-window nil "Should be selectable via other window.")
  (treemacs-no-delete-other-windows
   t
   "Don't delete when running delete other windows.")
  (treemacs-eldoc-display t "Show eldoc.")
  (treemacs-show-hidden-files t "Show hidden files in Treemacs.")
  (treemacs-follow-after-init t "Don't follow after initialization.")
  (treemacs-directory-name-transformer
   #'identity
   "Function to use for transforming directory names.")
  (treemacs-file-event-delay 5000 "Delay between monitoring for file events.")
  (treemacs-file-follow-delay 0.2 "Delay between following files.")
  (treemacs-tag-follow-delay 1.5 "Delay between following tags.")
  (treemacs-deferred-git-apply-delay
   0.5
   "Delay before running git for updates.")
  (treemacs-max-git-entries 5000 "Cap out the number of Git entries.")
  (treemacs-goto-tag-strategy
   'refetch-index
   "Strategy to use for jumping to tag.")
  (treemacs-indentation 2 "Set Treemacs indentation amount.")
  (treemacs-indentation-string " " "String to use for Treemacs indentation.")
  (treemacs-missing-project-action 'ask "Prompt when missing project.")
  (treemacs-file-extension-regex
   treemacs-last-period-regex-value
   "Regex to use to match file extension.")
  (treemacs-read-string-input 'from-child-frame "Use child frame for input.")
  (treemacs-show-cursor nil "Don't show the cursor in the Treemacs window.")
  (treemacs-silent-filewatch nil "Watch files loudly.")
  (treemacs-silent-refresh nil "Refresh Treemacs loudly.")
  (treemacs-space-between-root-nodes t "Add space between root nodes.")
  (treemacs-user-mode-line-format nil "Don't format.")
  (treemacs-user-header-line-format nil "Don't format.")
  (treemacs-workspace-switch-cleanup nil "Don't cleanup on workspace switch")
  (treemacs-tag-follow-cleanup t "Cleanup when following a tag.")
  (treemacs-project-follow-cleanup
   nil
   "Don't cleanup when following project switch")
  (treemacs-recenter-distance 0.1 "Amount to recenter.")
  (treemacs-recenter-after-file-follow nil "Don't recenter.")
  (treemacs-recenter-after-tag-follow nil "Don't recenter.")
  (treemacs-recenter-after-project-jump 'always "Always recenter.")
  (treemacs-recenter-after-project-expand 'on-distance "Sometimes recenter.")
  (treemacs-move-forward-on-expand
   nil
   "Don't move selection forward on expansion.")
  (treemacs-no-png-images nil "Use PNG images if available."))

(use-package treemacs-magit
  :after
  treemacs
  magit)

(use-package treemacs-projectile
  :after
  treemacs
  projectile)

(use-package treemacs-all-the-icons
  :after
  treemacs
  all-the-icons)

(use-package treemacs-icons-dired
  :after
  treemacs
  dired

  :hook
  ((dired-mode) . treemacs-icons-dired-mode))

(use-package lsp-treemacs
  :after
  lsp-mode
  treemacs

  :diminish
  lsp-treemacs-sync-mode

  :hook
  ((lsp-mode) . lsp-treemacs-sync-mode)

  :bind
  (:map lsp-mode-map
        ("C-c l T s" . hgs--toggle-lsp-treemacs-symbols))

  :init
  (defun hgs--toggle-lsp-treemacs-symbols (&optional state)
    "Toggle the `lsp-treemacs-symbols' window.
Can be forced on by supplying >0 or t, and off via <0."
    (interactive)
    (save-selected-window
      (let* ((symbols-buffer (get-buffer "*LSP Symbols List*"))
             (symbols-window (get-buffer-window symbols-buffer))
             (open #'lsp-treemacs-symbols)
             (close (lambda ()
                      (delete-window symbols-window)))
             (toggle (lambda ()
                       (if (and
                            (not (null symbols-buffer))
                            (not (null symbols-window)))
                           (funcall close)
                         (lsp-treemacs-symbols)))))
        ;; If we haven't been passed an argument
        (if (null state)
            (funcall toggle)
          ;; If we have been passed an argument
          (if (or (eq state t) (> state 0))
              ;; If it's trying to be toggled on
              (funcall open)
            ;; If it's trying to be toggled off
            (funcall close)))))))

(use-package dap-mode
  ;; (use-package dap-LANGUAGE) ; To load dap adapter for LANGUAGE

  :hook
  (dap-mode . tooltip-mode)
  (dap-mode . dap-ui-mode)
  (dap-mode . dap-tooltip-mode)
  (dap-mode . dap-ui-controls-mode)

  :custom
  (dap-auto-configure-features
   '(sessions locals controls tooltip)
   "Which DAP features should be auto-configured by default.")
  (dap-breakpoints-file
   (concat hgs-cache-directory "dap-breakpoints")
   "Place DAP break-point information into the cache."))

(use-package docker
  :commands
  docker

  :bind
  (:map global-map
        ("C-c D" . docker))

  :init
  (defun hgs-toggle-docker-as-root ()
    "Toggle whether to run Docker as root on/off."
    (interactive)
    (message "`docker-run-as-root' is now %s" docker-run-as-root)
    (if (null docker-run-as-root)
        (setq docker-run-as-root t)
      (setq docker-run-as-root nil))))

(use-package doom-modeline
  :after
  all-the-icons

  :commands
  doom-modeline-mode

  :hook
  ((window-setup-hook after-init) . doom-modeline-mode))

;; Local Variables:
;; mode: emacs-lisp
;; coding: utf-8-unix
;; fill-column: 80
;; require-final-newline: t
;; End:

;;; core-config.el ends here
