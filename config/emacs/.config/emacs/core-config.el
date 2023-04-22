;;; core-config.el --- Core emacs configurations -*- lexical-binding: t; -*-

;;; Commentary:

;; Performs base-line Emacs and package configuration upon which the end-user
;; can build.

;;; Code:

(require 'cl-lib)
(require 'files)
(require 'custom)
(require 'use-package)

(let ((default-directory (concat (file-name-directory load-file-name)
                                 "lisp")))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))
(require 'hgs-core)

;; Personal functions

(defun hgs-indent-buffer ()
  "Indent the current buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(defun hgs-byte-compile-configuration ()
  "Byte (re)compile our configuration scripts."
  (interactive)
  (dolist (file (directory-files-recursively hgs-emacs-config-directory ".*.el$"))
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
  "Predicate determining if the current display is of portrait dimensions.
This won't work as expected under daemon mode."
  (if (< (display-pixel-width) (display-pixel-height))
      t
    nil))

(defun hgs-monitor-is-horizontal ()
  "Predicate determining if the current display is of landscape dimensions.
This won't work as expected under daemon mode."
  (not (hgs-monitor-is-portrait)))

(defun hgs-toggle-window-dedicated ()
  "Toggle currently active window's dedication state."
  (interactive)
  (let ((window (get-buffer-window (current-buffer))))
    (if (set-window-dedicated-p window (not (window-dedicated-p window)))
        (message "Dedicated window for buffer: '%s'" (current-buffer))
      (message "Undedicated window for buffer: '%s'" (current-buffer)))))

;; Internal/built-in package configuration
;; Configure things defined in the core-most Emacs C code.
;; If you aren't sure where something goes, prefer to put it here instead of in
;; global scope.
(use-package emacs
  :no-require t ; Don't load `emacs.el' (obviously - it won't work)

  :functions
  hgs--setup-default-fonts
  hgs--setup-default-tui
  hgs--setup-default-gui
  hgs--suspend-frame

  :init
  (defun hgs--setup-default-fonts (frame)
    "Setup default fonts for the given frame."
    (cl-flet* ((get-font-family (lambda (font) (car font)))
              (get-font-size (lambda (font) (cdr font)))
              (set-font-override
               (lambda (font-type char-set &optional add)
                 "Override a specific region of code-points with the given font.

`FONT-TYPE' should be '(NAME . SIZE), and `CHAR-SET' should be
either a character set from `list-character-sets' or a script
symbol from `script-representative-chars'. `ADD' can be either
nil, 'prepend or 'append."
                 (set-fontset-font t    ; Default font-set
                                   char-set
                                   (font-spec :family (get-font-family font-type)
                                              :size (get-font-size font-type)
                                              :slant 'normal
                                              :weight 'normal)
                                   frame add))))
      (let ((latin-font '("DejaVu Sans Mono" . 12))
            ;; These unicode fonts can be a bit small relatively speaking
            (unicode-font '("Noto Sans Mono" . 14))
            (emoji-font '("Noto Color Emoji" . 14))
            (jp-font '("Source Han Sans JP" . 14))
            (kr-font '("Noto Sans CJK KR" . 14))
            (zh-font '("Noto Sans CJK SC" . 14)))
        ;; HACK: For some reason, some GUI Emacs instances will use underline
        ;; and no slant for italics. Probably something to do with my theme. To
        ;; counter this we manually turn off underline and add italic slant.
        (set-face-attribute 'italic frame :slant 'italic :underline nil)

        ;; Set the default frame font here to our latin font. This always gets
        ;; used before any other font in the default font-set.
        (set-face-attribute 'default frame
                            :family (get-font-family latin-font)
                            ;; Height is 1/10th size
                            :height (* 10 (get-font-size latin-font))
                            :width 'normal
                            :weight 'normal
                            :slant 'normal)

        ;; Now we override specific Unicode character sets. Order is important.
        ;; The earlier fonts in the font-set are preferred in the case of
        ;; overlapping code-points (and unicode overlaps a lot around CJK).
        ;;
        ;; Setup emoji
        (dolist (char-set-or-script '(symbol))
          ;; It's the first font, so we overwrite the existing font-set by
          ;; not passing 'append/'prepend
          (set-font-override emoji-font char-set-or-script))
        ;; Simplified Chinese
        (dolist (char-set-or-script '(chinese-gbk))
          (set-font-override zh-font char-set-or-script 'prepend))
        ;; Korean
        (dolist (char-set-or-script '(hangul))
          (set-font-override kr-font char-set-or-script 'prepend))
        ;; Japanese
        (dolist (char-set-or-script '(cjk-misc kana japanese-jisx0213.2004-1))
          (set-font-override jp-font char-set-or-script 'prepend))
        ;; Unicode font as the last font in the font-set for any stragglers
        (dolist (char-set-or-script '(unicode))
          (set-font-override unicode-font char-set-or-script 'append)))))

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
  (setq read-process-output-max (* 1024 1024))

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
        (concat hgs-emacs-state-directory "games/shared"))

  ;; Enable narrow-to-* family of functions, as they're disabled by default
  (put 'narrow-to-region 'disabled nil)
  (put 'narrow-to-defun 'disabled nil)
  (put 'narrow-to-page 'disabled nil)

  (bind-keys :map global-map
             ;; Prevent us from accidentally suspending frame in non-graphical
             ;; displays.
             ([remap suspend-frame] . hgs--suspend-frame)

             ;; Replace the case altering bindings with their dwim variants to
             ;; make life easier for free!
             ([remap upcase-word] . upcase-dwim)
             ([remap downcase-word] . downcase-dwim)
             ([remap capitalize-word] . capitalize-dwim))

  ;; Indicate the depth of recursion of the mini-buffer in the mode-line
  (minibuffer-depth-indicate-mode +1)

  :custom
  (require-final-newline t "We should always require a final newline.")
  (completion-ignore-case t "Ignore case for built-in completion matching in
general.")
  (read-file-name-completion-ignore-case t "Ignore case for file name completion
matching.")
  (read-buffer-completion-ignore-case t "Ignore case for buffer name completion
matching.")
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
  (auto-save-list-file-prefix (concat hgs-emacs-state-directory
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
   ;; Don't bother truly attempting to tame Emacs windowing. True freedom is
   ;; learning to not care where things are as long as *Help* doesn't replace
   ;; you current buffer ;).
   ;;
   ;; Note: Be very careful about this structure. Breaking it breaks Emacs'
   ;; multiplexing.
   ;;
   ;; General Structure:
   ;; (REGULAR-EXPRESSION-STRING
   ;;  ORDERED-LIST-OF-WINDOW-SELECTION-FUNCTIONS
   ;;  OPTIONS...)
   `(("^\\*\\([Hh]elp\\|[Cc]ustom\\|[Ff]aces\\).*"
      (display-buffer-reuse-mode-window)
      (inhibit-same-window . t))
     ("^\\*\\([Cc]ompilation\\|[Cc]ompile-[Ll]og\\|[Ww]arnings\\).*"
      (display-buffer-reuse-mode-window display-buffer-in-previous-window)
      (inhibit-same-window . t))
     ("^\\*[Mm]essages.*"
      (display-buffer-reuse-mode-window)
      (inhibit-same-window . t))
     ("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
      ()
      (window-parameters . (mode-line-format . none))))
   "Custom overrides for window placement of specific buffers.")
  (display-buffer-base-action
   ;; Defaults to make Emacs let us control most of the window layout. This
   ;; makes Emacs prefer to use existing windows or the same window rather than
   ;; creating new splits by default. This helps keep things relatively
   ;; deterministic (and hence learnable), while at the same time giving us more
   ;; opportunity to decide to make splits & frames ourselves.
   '((display-buffer-reuse-window
      display-buffer-reuse-mode-window
      display-buffer-same-window
      display-buffer-in-previous-window)
     . ())
   "Default action for window placement in order to make Emacs multiplexing more
predictable."))

(use-package minibuffer
  :bind
  ;; Allows us to use a narrowing framework for capf completions, which in my
  ;; opinion is better UX than company-mode.
  ("C-c ;" . completion-at-point))

;; Only needed for stopping some Emacs games from littering configuration
;; directory
(use-package gamegrid
  :config
  (setq gamegrid-user-score-file-directory
        (concat hgs-emacs-state-directory "games/gamegrid")))

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

(use-package pulse
  :init
  ;; Note this currently is effectively constant
  (defcustom hgs--line-pulse-commands
    '(scroll-up-command
      scroll-down-command
      recenter-top-bottom
      other-window)
    "Commands which should be considered pulse-worthy events."
    :type '(repeat symbol)
    :group 'personal)

  (defun hgs--pulse-current-line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point)))

  (defun hgs--add-pulse-advice ()
    "Enable Emacs pulsing the current line on certain jump navigation events."
    (dolist (command hgs--line-pulse-commands)
      (advice-add command :after #'hgs--pulse-current-line)))

  (defun hgs--remove-pulse-advice ()
    "Disable Emacs pulsing the current line on certain jump navigation events."
    (dolist (command hgs--line-pulse-commands)
      (advice-remove command :after #'hgs--pulse-current-line)))

  (hgs--add-pulse-advice)

  :custom
  (pulse-delay 0.03 "Delay between pulse iterations (seconds).")
  (pulse-iterations 10 "Number of iterations per pulse."))

(use-package find-file
  :custom
  (ff-case-fold-search t "Ignore case in matching.")
  (ff-always-in-other-window nil "Don't open other file in other window always")
  (ff-ignore-include
   nil "Use special automation for jumping to include under cursor.")
  (ff-always-try-to-create
   nil "Don't try to create the other file if it doesn't exist.")
  (ff-quiet-mode t "Don't trace searched directories."))

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
    :hook
    ((prog-mode text-mode) . hl-line-mode)))

(unless (version<= emacs-version "27.1.0")
  (use-package tab-bar
    :diminish
    tab-bar-mode

    :hook
    (after-init . tab-bar-mode)

    :custom
    (tab-bar-show nil "Don't show the tab bar. Rely on the mode-line to display
current tab, and using `tab-switch' for searching tabs.")
    (tab-bar-new-tab-choice t "Start new tab on current buffer.")))

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
  hgs--reset-whitespace-mode
  hgs--reset-whitespace-mode-local-hack

  :hook
  ((prog-mode text-mode) . hgs--reset-whitespace-mode-local-hack)

  :config
  (put 'whitespace-line-column 'safe-local-variable #'integerp)

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
   `((".*" ,(concat hgs-emacs-state-directory) "backup")
     t)
   "Put autosaves in the state directory.")
  (backup-directory-alist
   `((".*" . ,(concat hgs-emacs-state-directory "backup")))
   "Put backups in the state directory.")
  (backup-by-copying t "Always copy rather than symlink for backups."))

(use-package subword
  :diminish
  global-subword-mode
  subword-mode

  :commands
  global-subword-mode

  :hook
  ((prog-mode text-mode special-mode minibuffer-setup) . subword-mode))

(use-package recentf
  :diminish
  recentf

  :hook
  ((prog-mode text-mode special-mode) . recentf-mode)

  :custom
  (recentf-save-file (concat hgs-emacs-state-directory "recentf")
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
  :functions
  hgs-kill-dired-buffers
  hgs--dired-up-directory-clean

  :bind
  ((:map global-map
         ("C-x d" . dired))
   (:map dired-mode-map
         ("^" . hgs--dired-up-directory-clean)))

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

  :bind
  (:map winner-mode-map
        ("C-c <left>" . winner-undo)
        ("C-c <right>" . winner-redo))

  :hook
  ((prog-mode text-mode special-mode) . winner-mode))

(use-package desktop
  :config
  (setq desktop-dirname (concat hgs-emacs-state-directory "desktop"))

  :custom
  (desktop-base-file-name "autosave" "Name of the desktop save file.")
  (desktop-base-lock-name "autosave-lock" "Name of the lock for desktop
package."))

(use-package savehist
  :diminish
  savehist-mode

  :hook
  (after-init . savehist-mode)

  :custom
  (savehist-file (concat hgs-emacs-cache-directory "history")
                 "Cache history in cache directory."))

(use-package eww
  :bind
  (:map search-map
        ("M-w" . eww-search-words))

  :custom
  (eww-bookmarks-directory
   (concat hgs-emacs-state-directory "eww")
   "Place eww bookmarks under the state directory."))

(use-package tramp
  :custom
  (tramp-auto-save-directory
   (concat hgs-emacs-state-directory "tramp-auto-save")
   "Directory to place auto-saves when editing via Tramp.")
  (tramp-backup-directory-alist
   backup-directory-alist
   "Put Tramp backups in the same place as local backups.")
  (tramp-persistency-file-name
   (concat hgs-emacs-cache-directory "tramp-persistency.el")
   "Put the Tramp persistency file in the cache directory.")
  (tramp-histfile-override
   nil "We don't want to store remote shell history locally."))

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
  (eshell-directory-name (concat hgs-emacs-state-directory "eshell")
                         "Use state directory for storing files (e.g. aliases,
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

  :hook
  (((text-mode) . flyspell-mode)
   ((prog-mode) . flyspell-prog-mode))

  :custom
  (ispell-program-name "aspell" "Spellcheck program to use.")
  (ispell-extra-args
   '("--sug-mode=ultra" "--lang=en_US")
   "Change the default lookup mode for performance, and force language to
American English."))

(use-package epa
  :hook
  (((find-file) . hgs--epa-inhibit-backups))

  :init
  (defun hgs--epa-inhibit-backups ()
    "Inhibit backups when operating on encrypted files."
    (when (and buffer-file-name
               (string-match epa-file-name-regexp buffer-file-name))
        (message "Backup inhibited for this file `%s'." buffer-file-name)
        (setq-local backup-inhibited t)))

  :custom
  (epa-file-inhibit-auto-save
   t
   "Don't autosave when operating on encrypted files."))

(use-package erc
  :defines
  hgs-erc-freenode-server-regexp
  hgs-erc-rizon-server-regexp
  hgs-erc-libera-server-regexp
  hgs-erc-oftc-server-regexp

  :functions
  erc
  erc-tls
  hgs--erc-save-buffers-to-logs
  hgs--erc-disable-whitespace-mode
  hgs-erc-current-network-name

  :hook
  (erc-mode . hgs--erc-disable-whitespace-mode)
  ;; Default to readonly when joining a channel to prevent fat fingering by
  ;; default
  (erc-join . read-only-mode)

  :init
  (require 'rx)
  (defcustom hgs-erc-freenode-server-regexp
    (rx "freenode")
    "Regular expression for matching Freenode IRC server name."
    :group 'personal
    :type 'regexp)
  (defcustom hgs-erc-rizon-server-regexp
    ;; Rizon uses a round-robin allocation system, so we might get a different
    ;; actual server domain each time we connect via the generic Rizon domain
    ;; name.
    (rx (or "rizon"
            "sacredland.world"
            "hostsailor.com"
            "uworld.se"
            "anonchan.com"
            "defineya.com"
            "losslessone.com"
            "sxci.net"
            "shells.org"))
    "Regular expression for matching Rizon IRC server name."
    :group 'personal
    :type 'regexp)
  (defcustom hgs-erc-libera-server-regexp
    (rx "libera")
    "Regular expression for matching Libera Chat IRC server name."
    :group 'personal
    :type 'regexp)
  (defcustom hgs-erc-oftc-server-regexp
    (rx "oftc")
    "Regular expression for matching OFTC IRC server name."
    :group 'personal
    :type 'regexp)

  (defun hgs--erc-disable-whitespace-mode ()
    "Disables whitespace mode in erc buffers, as it causes issues."
    (whitespace-mode -1))

  (defun hgs--erc-save-buffers-to-logs (&optional _)
    "Auto saves erc buffers to their log files when exiting emacs."
    (save-some-buffers t (lambda (&optional _)
                           (when (eq major-mode 'erc-mode)
                             t))))

  (defun hgs-erc-current-network-name (&optional buffer-to-check)
    "Returns network name (not necessarily server).
Will use the current buffer unless `BUFFER-TO-CHECK' is non-nil,
in which case it will be used."
    (require 'erc)
    (let ((channel-buffer (or buffer-to-check (current-buffer))))
      (with-current-buffer channel-buffer
        (or (and (fboundp 'erc-network-name) (erc-network-name))
            (erc-shorten-server-name
             (or erc-server-announced-name
                 erc-session-server))))))

  :config
  (add-to-list 'erc-modules 'notifications) ;; Enable notifications
  (add-to-list 'erc-modules 'spelling) ;; Enable spelling corrections
  (erc-update-modules)

  (advice-add #'save-buffers-kill-emacs :before #'hgs--erc-save-buffers-to-logs)

  :custom
  (erc-nick user-login-name "Default nick.")
  (erc-user-full-name erc-nick "Full real name of the user.")
  (erc-email-userid erc-nick "Email of the user.")
  (erc-nick-uniquifier
   "_"
   "Use trailing underscores for nicks that are already taken.")
  (erc-join-buffer
   'bury
   "Don't bring channel buffers to the forefront when they appear.")
  (erc-hide-list
   '()
   "Don't hide anything by default.")
  (erc-lurker-hide-list
   '("JOIN"
     "PART"
     "QUIT")
   "Hide status changes for lurkers.")
  (erc-lurker-threshold-time
   (* 60 (* 60 (* 24)))
   "Class anyone inactive for 24 hours as a lurker.")
  (erc-debug-log-file
   (concat hgs-emacs-state-directory "erc/debug.log")
   "Relocate the erc debug log file somewhere more sensible.")
  (erc-send-whitespace-lines
   nil
   "Don't send lines only consisting of whitespace.")
  (erc-rename-buffers
   t
   "Rename server buffers with network name instead of
server:port where possible. This is particularly useful for
bouncers, where you'll have multiple server buffers for different
networks, but they'll appear as the same server to the client."))

;; Manages joining channels (both manually and automatically)
(use-package erc-join
  :after
  erc

  :functions
  erc-autojoin-mode
  erc-autojoin-enable
  erc-autojoin-disable

  :hook
  (erc-mode . erc-autojoin-enable)

  :custom
  (erc-autojoin-channels-alist
   '()
   "AList of server -> channel list to auto-join on connection.")
  (erc-autojoin-timing
   'ident
   "Auto-join after successful identification with NickServ."))

(use-package erc-fill
  :after
  erc

  :functions
  erc-fill-mode
  erc-fill-enable
  erc-fill-disable

  :hook
  (erc-mode . erc-fill-enable)

  :custom
  (erc-fill-column
   fill-column
   "Set ERC fill column to be the same as everywhere else.")
  (erc-fill-function 'erc-fill-variable "Use variable filling")
  (erc-fill-prefix
   (make-string (erc-timestamp-offset) ? )
   "Prefix wrapping with enough spaces to be just past the timestamp
(aligning with nicks)."))

;; Highlights or hides messages matching certain patterns
(use-package erc-match
  :after
  erc

  :functions
  erc-match-mode
  erc-match-enable
  erc-match-disable

  :hook
  (erc-mode . erc-match-enable)

  :custom
  (erc-pals '() "Pals to highlight.")
  (erc-fools '() "Fools to ignore.")
  (erc-keywords '() "Keywords to track."))

;; Tracks active erc buffers
(use-package erc-track
  :after
  erc

  :functions
  erc-track-mode
  erc-track-enable
  erc-track-disable

  :hook
  (erc-mode . erc-track-enable)

  :custom
  (erc-track-enable-keybindings
   t
   "Use track mode keybindings (e.g. C-c SPC for jumping between IRC
buffers).")
  (erc-track-visibility
   'visible
   "Consider all actually visible frames as containing visible buffers.")
  (erc-track-exclude
   '()
   "Channels to exclude from tracking.")
  (erc-track-exclude-types
   '("JOIN"
     "NICK"
     "QUIT"
     "MODE"
     "AWAY")
   "Types of messages to exclude")
  (erc-track-exclude-server-buffer
   t
   "Don't track anything in the server buffer."))

;; Stores previous commands/text in a ring available for recall via M-p/M-n
(use-package erc-ring
  :after
  erc

  :functions
  erc-ring-mode
  erc-ring-enable
  erc-ring-disable

  :hook
  (erc-mode . erc-ring-enable))

;; Hides mode changes from the servers
(use-package erc-netsplit
  :after
  erc

  :functions
  erc-netsplit-mode
  erc-netsplit-enable
  erc-netsplit-disable

  :hook
  (erc-mode . erc-netsplit-enable))

;; Performs logging of channels
(use-package erc-log
  :after
  erc

  :functions
  erc-log-mode
  erc-log-enable
  erc-log-disable

  :hook
  (((erc-mode) . erc-log-enable))

  :init
  (defcustom hgs-erc-log-channels-directory
    (file-name-as-directory (concat hgs-emacs-state-directory "erc/logs"))
    "Base directory path for placing erc logs."
    :type 'directory
    :group 'personal)

  (defcustom hgs-erc-log-auto-create-directories
    t
    "Whether we should automatically create needed directories for erc log
files."
    :type '(choice (const :tag "Yes" t)
                   (const :tag "No" nil)
                   (const :tag "Ask" 'ask))
    :group 'personal)

  (defun hgs--erc-compute-log-channels-directory (_buffer
                                                  target nick server
                                                  _port)
    "This computes a path to logs for a specific nick, server and channel.
This results in a path based off of `hgs-erc-log-channels-directory', matching
the pattern of $nick/$server/$channel/."
    (defvar hgs-erc-log-channels-directory)
    (defvar hgs-erc-log-auto-create-directories)
    (let* ((channel (if target
                        target
                      "server-channel"))
           (directory
            (file-name-as-directory
             (concat hgs-erc-log-channels-directory
                     (format "%s/%s/%s" nick server channel)))))
      ;; We need to prompt to create the directory if it doesn't exist
      (cond
       ;; Prompt
       ((equal hgs-erc-log-auto-create-directories 'ask)
        (when (and (not (file-directory-p directory))
                   (yes-or-no-p (format "Create directory `%s' for logs?"
                                        directory)))
          (make-directory directory 'create-parents)))
       ;; Just do it quietly if true
       (hgs-erc-log-auto-create-directories
        (make-directory directory 'create-parents))
       (t (message "Missing directory for logs on %s with nick %s on server %s"
                   target nick server)))
      directory))

  (defun hgs--erc-generate-log-file-name (_buffer _target _nick _server _port)
    "This function computes a short log file name.
The name of the log file is composed of nick current date."
    (convert-standard-filename
     (concat (format-time-string "%Y-%m-%d") ".log")))

  :custom
  (erc-log-channels-directory
   #'hgs--erc-compute-log-channels-directory
   "Where to place the log files. Dynamically compute based on channel
information.")
  (erc-generate-log-file-name-function
   #'hgs--erc-generate-log-file-name
   "Use a minimal date format for the log files themselves.")
  (erc-log-insert-log-on-open
   t
   "Show log file up until now for the channel on open.")
  (erc-log-channels t "Log channel contents to disk.")
  (erc-log-write-after-send t "Save log file on send.")
  (erc-log-write-after-insert
   t
   "Save log file when new text appears in the buffer.")
  (erc-save-buffer-on-part t "Save log file when leaving a channel."))

;; Manages timestamps
(use-package erc-stamp
  :after
  erc

  :functions
  erc-stamp-mode
  erc-stamp-enable
  erc-stamp-disable

  :hook
  (((erc-mode) . erc-stamp-enable))

  :custom
  (erc-hide-timestamps nil "Timestamps should be visible.")
  (erc-timestamp-only-if-changed-flag nil "Always insert timestamp")
  (erc-insert-timestamp-function
   #'erc-insert-timestamp-left
   "Put timestamps on the left.")
  (erc-timestamp-format "[%H:%M:%S]" "How to present timestamps."))

;; Keeps the erc buffers to a manageable size
(use-package erc-truncate
  :after
  erc

  :functions
  erc-truncate-mode
  erc-truncate-enable
  erc-truncate-disable

  :hook
  (((erc-mode) . erc-truncate-enable))

  :custom
  (erc-max-buffer-size 30000 "Truncate buffers so they don't hog core.")
  (erc-truncate-buffer-on-save
   nil
   "Don't truncate the erc buffer after saving to log file."))

(use-package erc-backend
  :after
  erc

  :custom
  (erc-server-reconnect-attempts 5 "Try up to 5 times to reconnect.")
  (erc-server-reconnect-timeout 3 "Timeout after 3 minutes."))

(use-package erc-services
  :after
  erc

  :functions
  erc-services-mode
  erc-services-enable
  erc-services-disable

  :hook
  (erc-mode . erc-services-enable)

  :custom
  (erc-prompt-for-password
   nil
   "We don't want to prompt, instead we should use an auth-source.")
  (erc-prompt-for-nickserv-password
   nil
   "We don't want to prompt, instead we should always use an auth-source."))

(use-package url
  :custom
  (url-cache-directory
   (concat hgs-emacs-cache-directory "url")
   "Put the url package's cache directory where we would expect.")
  (url-configuration-directory
   (concat hgs-emacs-data-directory "url")
   "Put the url package's configuration directory in the data directory."))

(use-package bookmark
  :custom
  (bookmark-default-file
   (concat hgs-emacs-state-directory "bookmarks")
   "Put bookmarks in the data directory."))

(use-package custom
  :custom
  (custom-theme-directory
   (concat hgs-emacs-config-directory "themes")
   "Expect custom themes from our configuration directory."))

(use-package python
  :defines
  python-indent-guess-indent-offset

  :mode
  (("\\.py\\'" . python-mode))

  :custom
  (python-indent-offset 2 "We prefer to default indent in python to 2 space.")
  (python-indent-guess-indent-offset
   t
   "Guess indent offset and set it appropriately."))

;; C/C++
(use-package cc-mode
  :init
  (defcustom hgs-clang-format-command
    ;; (PROGRAM ARGS...)
    '("clang-format")
    "Command to run clang formatting."
    :type 'list
    :group 'personal)

  :config
  (reformatter-define clang-format
    :program (car hgs-clang-format-command)
    :args (cdr hgs-clang-format-command)
    :group 'cpp
    :lighter " CF"))

(use-package org
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
  (org-archive-location
   (concat hgs-org-directory "archive/%s::datetree/")
   "Place archivals into an organized datetree in an archive sub-directory.")
  (org-refile-targets
   '((nil : maxlevel . 9)
     (org-agenda-files :maxlevel . 9))
   "Allow us to refile to all Org files in the agenda and current buffer to an
arbitrary depth.")
  (org-refile-use-outline-path 'file "Show the file name as part of the outline
path when refiling.")
  (org-refile-allow-creating-parent-nodes 'confirm "Prompt when wanting to
create new nodes during a refile.")
  (org-outline-path-complete-in-steps nil "Allow us to complete the path using a
narrowing framework.")
  (org-log-into-drawer t "Place automatic log lines into a drawer.")
  (org-todo-keywords
   '((sequence
      "TODO(t)" "INPROGRESS(p!)" "BLOCKED(b@)" ; Non-terminal states
      "|"
      "DONE(d!)" "CANCELLED(c@)")) ; Terminal states
   "Sequence of states for org-todo entries.")
  (org-capture-templates
   `(("n" "Notes" entry
      (file
       (lambda ()
         org-default-notes-file))
      "* NOTES %? %^g\n%U" :empty-lines 1)
     ("t" "Todo" entry
      (file
       (lambda ()
         (concat org-directory "todo.org")))
      "* TODO %? %^g\n%U" :empty-lines 1)
     ("s" "Scheduled Todo" entry
      (file
       (lambda ()
         (concat org-directory "todo.org")))
      "* TODO %? %^g\nSCHEDULED: %^t\n%U" :empty-lines 1)
     ("d" "Deadline" entry
      (file
       (lambda ()
         (concat org-directory "todo.org")))
      "* TODO %? %^g\nDEADLINE: %^t" :empty-lines 1))
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

  :custom
  (org-indent-indentation-per-level
   2
   "Reduce indentation in `org-indent' mode."))

;; Personal lisp packages

;; HERE

;; Third-party package configuration

(use-package go-mode
  :mode
  (("\\.go\\'" . go-mode)))

(use-package rustic-mode
  :mode
  (("\\.rs\\'" . rustic-mode))

  :custom
  (rustic-lsp-setup-p nil "Don't automatically try to setup lsp-mode")
  (rustic-format-trigger 'on-save "Format Rust code on save."))

(use-package vimrc-mode
  :mode
  (("\\.vim\\'" . vimrc-mode)
   ("[._]?g?vimrc\\'" . vimrc-mode)
   ("\\.exrc\\'" . vimrc-mode)))

(use-package dockerfile-mode
  :mode
  (("Dockerfile\\'" . dockerfile-mode))

  :config
  ;; Allow the use to put a file-local variable specifying the image name.
  (put 'docker-image-name 'safe-local-variable #'stringp))

(use-package protobuf-mode
  :mode
  (("\\.\\(pb\\|proto\\)\\'" . protobuf-mode)))

(use-package bazel
  :mode
  (("/BUILD\\(\\..*\\)?\\'" . bazel-build-mode)
   ("/WORKSPACE\\'" . bazel-workspace-mode)
   ("\\.bzl\\'" . bazel-starlark-mode)
   ("\\.bazelrc\\'" . bazelrc-mode)))

(use-package meson-mode
  :mode
  (("\\.meson\\'" . meson-mode)
   ("/meson\\.build\\'" . meson-mode)))

(use-package jq-mode
  :after
  json-mode

  :defines
  json-mode-map

  :mode
  ("\\.jq\\'" . jq-mode)

  :bind
  (:map json-mode-map
        ("C-c C-j" . jq-interactively)))

(use-package toml-mode
  :mode
  (("\\.toml\\'" . toml-mode)))

;; Dim non-focused windows for clarity. This seems to cause massive terminal
;; slow-down when changing active window, so I might eventually remove it.
(use-package dimmer
  :demand t

  :diminish
  dimmer-mode

  :functions
  hgs--apply-dimmer-fix

  :init
  ;; Apply some fixes when/if these packages load to prevent dimmer from
  ;; interfering with their visibility.
  (defmacro hgs--apply-dimmer-fix (package-name)
    (let ((package-name-str (symbol-name package-name)))
    `(with-eval-after-load ,package-name-str
       (funcall (intern (format "dimmer-configure-%s" ,package-name-str))))))

  :config
  (dolist (pkg '(magit selectrum which-key org posframe gnus helm company-box))
    (hgs--apply-dimmer-fix pkg))

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
  avy-goto-word
  avy-goto-line

  :bind
  (:map global-map
        ("C-'" . avy-goto-char-timer))
  (:map isearch-mode-map
        ("C-'". avy-isearch))

  :config
  (defun avy-action-embark (pt)
    "Run `embark-act' on avy filtered candidates."
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  (setf (alist-get ?\; avy-dispatch-alist) #'avy-action-embark)

  :custom
  (avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l) "Use home row for Avy prompts.")
  (avy-keys-alist '() "Alist of Avy commands to keys to use for prompts. Falls
back to `avy-keys'")
  (avy-style 'at "Overlay display style.")
  (avy-styles-alist '() "Alist of Avy commands to overlay display styles.")
  (avy-background nil "Use a gray background during selection.")
  (avy-all-windows t "Scan all windows on the selected for selection.")
  (avy-case-fold-search t "Ignore case for search.")
  (avy-single-candidate-jump nil "Don't auto-jump on single candidates to allow applying
actions.")
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

(use-package org-mime
  :custom
  (org-mime-library 'mml "Use mml."))

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
     "CPATH"
     "MANPATH"
     "INFOPATH"
     "FPATH"
     "NAME"
     "EMAIL"
     "LANG"
     "LC_ALL"
     "PASSWORD_STORE_DIR"
     ;; User-defined variables
     )
   "All the shell variables Emacs should be attempting to source."))

(use-package auth-source
  :demand t

  :custom
  (auth-sources
   `(;; PGP encryped authinfo format
     (:source ,(concat hgs-emacs-data-directory "authinfo.gpg")))
   "Setup my ordered list of preferred authentication sources for Emacs."))

(use-package auth-source-pass
  :if (or (executable-find "pass")
          (executable-find "gopass"))

  :after
  auth-source

  :functions
  hgs-pass-refresh-store-directory

  :config
  (defun hgs-refresh-password-store-directory ()
    "Refetch the store directory variable from the hosting shell environment."
    (exec-path-from-shell-initialize)
    (customize-set-variable 'auth-source-pass-filename
                            (getenv "PASSWORD_STORE_DIR")))

  ;; Adds auth source to list
  (auth-source-pass-enable)

  :custom
  (auth-source-pass-filename (getenv "PASSWORD_STORE_DIR"))
  (auth-source-pass-port-separator
   ":"
   "Separator between host name and port in an entry."))


(use-package password-store
  :if (or (executable-find "pass")
          (executable-find "gopass"))

  :after
  auth-source-pass

  :custom
  (password-store-executable
   (if (executable-find "gopass")
       "gopass"
     "pass")
   "Prefer gopass if it is available."))

(use-package pass
  :if (or (executable-find "pass")
          (executable-find "gopass"))

  :after
  password-store

  :commands
  pass)

(use-package vertico
  :demand t

  :diminish
  vertico-mode

  :hook
  (after-init . vertico-mode)

  :init
  (defun hgs--vertico-completion-in-region (&rest args)
    "Use `consult-completion-in-region' if Vertico and Consult are available.
Otherwise use the default `completion--in-region' function."
    (apply (if (and vertico-mode (fboundp #'consult-completion-in-region))
               #'consult-completion-in-region
             #'completion--in-region)
           args))

  :custom
  (completion-in-region-function #'hgs--vertico-completion-in-region
                                 "Use custom `completion-in-region' function.")
  (vertico-scroll-margin 0 "Show no scroll margin.")
  (vertico-resize t "Dynamically resize minibuffer.")
  (vertico-count 15 "Show more candidates.")
  (vertico-cycle t "Enable cycling through candidate list."))

(use-package orderless
  :demand t

  :custom
  (completion-styles
   '(orderless basic)
   "Use orderless completion style. `basic' must be provided as a fallback in
order to ensure dynamic completion tables work correctly.")
  (completion-category-defaults
   nil
   "Disable all the default per-category completion styles.")
  (completion-category-overrides
   '((file (styles basic partial-completion)))
   "Enable partial completion for files for wildcard & partial path matching
support.")
  (orderless-matching-styles
   '(orderless-initialism orderless-literal orderless-regexp)
   "List of matching styles to use for orderless completion"))

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
        ("M-s e" . consult-isearch-history)
        ("C-x K" . consult-kmacro)
        ("C-M-#" . consult-register)
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
        ("f" . consult-flycheck)
        ("F" . consult-flymake)
        ("o" . consult-outline)
        ("m" . consult-mark)
        ("k" . consult-global-mark)
        ("i" . consult-imenu)
        ("I" . consult-imenu-multi))
  (:map search-map
        ("f" . consult-find)
        ("L" . consult-locate)
        ("g" . consult-grep)
        ("G" . consult-git-grep)
        ("r" . consult-ripgrep)
        ("l" . consult-line)
        ("L" . consult-line-multi)
        ("o" . occur)
        ("m" . multi-occur)
        ("k" . consult-keep-lines)
        ("u" . consult-focus-lines))
  (:map isearch-mode-map
        ([remap isearch-edit-string] . consult-isearch-history)
        ("M-s l" . consult-line)
        ("M-s L" . consult-line-multi))

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
   #'project-root
   "Use built-in project.el for finding the project root."))

(use-package marginalia
  :after
  vertico

  :demand t

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
  hgs--embark-prefix-map
  embark-file-map

  :bind
  (:prefix "C-c e"
           :prefix-map hgs--embark-prefix-map
           :prefix-docstring "Embark commands"
           ;; You can access most fundamental embark operations such as
           ;; export/become/collect/live via act, so there's no need to rebind
           ;; them here.
           ;;
           ;; Contextual actions on object at point
           ("a" . embark-act)
           ;; Do the default action to thing at point
           ("d" . embark-dwim))

  (:map global-map
   ;; Improved bindings help
   ([remap describe-bindings] . embark-bindings))
  (:map minibuffer-local-map
        ;; Convenience bindings for inside a minibuffer. My general thoughts are
        ;; that beyond act/become which are frequent and short operations one
        ;; shouldn't need shortcuts for embark in the minibuffer.
        ("C-;" . embark-act)
        ("C-:" . embark-dwim))
  (:map embark-file-map
        ("j" . dired-jump))

  :init
  (which-key-add-key-based-replacements
    "C-c e" "Embark")

  :custom
  ;; (prefix-help-command
  ;;  #'embark-prefix-help-command
  ;;  "Replace the built-in prefix help command with a completing read version.")
  (embark-quit-after-action
   nil
   "Don't leave the minibuffer etc. after acting. Use prefix to quit.")
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
  (require 'async-bytecomp)
  (setq async-byte-compile-log-file
        (concat hgs-emacs-state-directory "async-bytecomp.log"))

  :hook
  ((after-init . async-bytecomp-package-mode)
   (dired-mode . dired-async-mode)))

(use-package transient
  :custom
  (transient-levels-file
   (concat hgs-emacs-state-directory "transient/levels.el")
   "Where to place the Transient levels file.")
  (transient-values-file
   (concat hgs-emacs-state-directory "transient/values.el")
   "Where to place the Transient values file.")
  (transient-history-file
   (concat hgs-emacs-state-directory "transient/history.el")
   "Where to place the Transient history file."))

(use-package pdf-tools
  :magic
  ("%PDF" . pdf-view-mode)

  :config
  ;; This performs a build step with some dependencies. I have noticed that it
  ;; tends to fail silently if invoked non-interactively and dependencies are
  ;; missing. Unfortunately this blows the default pdf-view out of the water, so
  ;; we'll prefer it anyway. For similar TTY reasons, the build step can appear
  ;; silent as well. If it doesn't work automatically, then run it manually, or
  ;; disable the package.
  (pdf-tools-install :no-query)

  :custom
  (pdf-view-display-size 'fit-width "Default to fit-to-width."))

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
  ws-butler-global-mode

  :hook
  ((text-mode prog-mode) . ws-butler-mode)

  :custom
  (ws-butler-keep-whitespace-before-point
   nil
   "Delete whitespace before point."))

(use-package expand-region
  :bind
  (:map global-map
        ("C-=" . er/expand-region)))

(use-package corfu
  :init
  (defun hgs-corfu-move-to-minibuffer ()
    "Transfer corfu completion to the minibuffer."
    ;; Corfu hijacks the completion in completion-in-region/at-point capf
    ;; entrypoints, so using this is a way to go from corfu to minibuffer for
    ;; consult. It may be preferable in the future to remove corfu in favor of
    ;; minibuffer completion only.
    (interactive)
    (let ((completion-extra-properties corfu--extra)
          completion-cycle-threshold completion-cycling)
      (apply #'hgs--vertico-completion-in-region completion-in-region--data)))

  :hook
  (prog-mode . corfu-mode)

  :bind
  (:map corfu-map
        ("M-m" . hgs-corfu-move-to-minibuffer))

  :config
  ;; There is no point in hijacking C-p/n when M-p/n are already used
  (unbind-key [remap previous-line] corfu-map)
  (unbind-key [remap next-line] corfu-map)

  ;; The whole point of using something like Corfu is:
  ;;  * To narrow via typing.
  ;;  * To cycle a couple of times to select specific entries.
  ;; You should never, ever really need to do anything else.
  ;;
  ;; The minibuffer serves as a better place for scanning through very long
  ;; lists than a pop-up.
  (unbind-key [remap beginning-of-buffer] corfu-map)
  (unbind-key [remap end-of-buffer] corfu-map)
  (unbind-key [remap scroll-up-command] corfu-map)
  (unbind-key [remap scroll-down-command] corfu-map)

  :custom
  (corfu-cycle t "Enable cycling through corfu candidate set.")
  (corfu-auto t "Enable auto-completion.")
  (corfu-auto-delay 0.5 "Delay for auto-complete in seconds.")
  (corfu-auto-prefix 3 "Character prefix length before auto-complete occurs.")
  (corfu-quit-no-match 'separator "Quit eagerly if no match to avoid popup
getting in the way.")
  (corfu-preselect-first nil "Don't preselect a candidate.")
  (corfu-echo-documentation t "Show documentation of completion in echo area.")
  (corfu-on-exact-match 'insert "Insert on an exact match.")
  (corfu-quit-at-boundary nil "Stay alive even if there is no match.")
  ;; Note: M-SPC is already hijacked under Gnome for some other purpose
  (corfu-separator ?\s "Use M-SPC as the separator.")
  (corfu-preview-current nil "Preview the currently selected candidate.")
  (corfu-scroll-margin 2 "Show small scroll margin."))

(use-package cape
  ;; This is a cool package for providing some good default CAPF backends and
  ;; converting company backend to CAPF compatible backends, but at the moment
  ;; we don't really need it. We load it anyway. May remove at a later date.
  )

(when hgs-has-dynamic-module-support
  (use-package tree-sitter
    :diminish
    global-tree-sitter-mode
    tree-sitter-mode
    tree-sitter-hl-mode

    :commands
    global-tree-sitter-mode
    tree-sitter-mode
    tree-sitter-hl-mode

    :hook
    ;; I would like to just use prog-mode and ignore the message it outputs when
    ;; there isn't a grammar found, but unfortunately that seems to hang
    ;; daemonized Emacs. We instead just swallow errors via a proxy function.
    ;; The alternative is to toggle these per language pair which is not the
    ;; most ergonomic at scale.
    ((prog-mode) . hgs--tree-sitter-modes)

    :init
    (defun hgs--tree-sitter-modes (&optional arg)
      "Proxy to `tree-sitter-mode' & `tree-sitter-hl-mode' swallowing errors."
      (let ((debug-on-error nil))
        (with-demoted-errors "Silenced error: %s"
          (tree-sitter-mode arg)
          (tree-sitter-hl-mode arg)))))

  (use-package tree-sitter-langs
    :after
    tree-sitter))

(use-package yasnippet
  :diminish
  yas-global-mode
  yas-minor-mode

  :commands
  yas-global-mode

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
   `(,(concat hgs-emacs-config-directory "snippets"))
   "Where to find snippet definitions."))

(use-package yasnippet-snippets
  :after yasnippet

  :config
  (yasnippet-snippets-initialize))

(use-package eglot
  :commands
  eglot

  :hook
  ;; Mostly we can get away with using manual invocation where needed
  ;; per-session
  ((c-mode c++-mode python-mode) . eglot-ensure)

  :bind
  (:map eglot-mode-map
        ;; None as of yet
        )

  :custom
  (eglot-autoreconnect 3 "Only attempt reconnect to LSP server if the previous
connection lasted at least N seconds.")
  (eglot-connect-timeout 30 "Time given before a connection should be considered
timed-out.")
  (eglot-sync-connect 3 "Synchronously block UI for only up to N seconds on
connect.")
  (eglot-events-buffer-size 2000000 "Maximum number of characters allowed in the
Eglot events buffer.")
  (eglot-autoshutdown nil "Don't shutdown LSP servers automatically when there
are no more using buffers. Prefer `eglot-shutdown'.")
  (eglot-extend-to-xref nil "Don't transiently consider out-of-project files you
jump to as part of the current workspace.")
  (eglot-confirm-server-initiated-edits t "Ask for confirmation when the server
tries to alter buffers.")
  (eglot-ignored-server-capabilities
   '()
   "List of Eglot capabilities you want to turn off."))

(use-package which-key
  :diminish
  which-key-mode

  :commands
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
  flip-frame
  flop-frame
  rotate-frame
  rotate-frame-anticlockwise

  :bind
  (:map global-map
        ("C-c <up>" . transpose-frame)
        ("C-c <down>" . rotate-frame-clockwise)))

(use-package rainbow-delimiters
  :diminish
  rainbow-delimiters-mode

  :hook
  ((prog-mode) . rainbow-delimiters-mode))

(use-package cmake-mode
  :mode
  (("\\(CMakeLists\\.txt\\|\\.cmake\\)\\'" . cmake-mode)))

(use-package yaml-mode
  :mode
  (("\\(\\.[Yy][Mm][Ll]\\|\\.yaml\\)\\'" . yaml-mode)))

(use-package csv-mode
  :mode
  (("\\.[Cc][Ss][Vv]\\'" . csv-mode)))

(use-package lua-mode
  :mode
  (("\\.lua\\'" . lua-mode))

  :custom
  (lua-indent-level 2 "We prefer to indent in Lua to 2 spaces."))

(use-package markdown-mode
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\(\\.md\\|\\.markdown\\)\\'" . markdown-mode)))

(use-package restclient
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
    (let* ((choices
            `(("UPPER_CASE" .
               ,#'string-inflection-upcase-function)
              ("snake_case" .
               ,#'string-inflection-underscore-function)
              ("Snake_Case" .
               ,#'string-inflection-capital-underscore-function)
              ("PascalCase" .
               ,#'string-inflection-pascal-case-function)
              ("camelCase" .
               ,#'string-inflection-camelcase-function)
              ("kebab-case" .
               ,#'string-inflection-kebab-case-function)))
           (completion-table
            (lambda (string predicate action)
              (cond
               ((eq action 'metadata)
                `(metadata
                  (category . symbol)
                  (annotation-function . ,#'identity)
                  (cycle-sort-function . ,#'identity)
                  (display-sort-function . ,#'identity)))
               (t (complete-with-action action choices string predicate))))))
      (save-mark-and-excursion
        (atomic-change-group
          (let* ((action
                  (cdr (assoc (completing-read "Select target style: "
                                               completion-table
                                               nil
                                               'require-match)
                              choices)))
                 (selection (string-inflection-get-current-word)))
            ;; Replace region with result of restyle
            (insert (funcall action selection))))))))

(use-package editorconfig
  :diminish
  editorconfig-mode

  :hook
  ((prog-mode) . editorconfig-mode))

(use-package wgrep
  :bind
  (:map grep-mode-map
        ("C-c C-p" . wgrep-change-to-wgrep))

  :custom
  (wgrep-enable-key "\C-c\C-p" "Key to make the grep buffer `wgrep' editable.")
  (wgrep-auto-save-buffer
   nil
   "Don't save buffer automatically when `wgrep-finish-edit'.")

  (wgrep-change-readonly-file
   nil
   "Don't apply changes regardless of whether the buffer is read-only."))

(when hgs-has-dynamic-module-support
  (use-package vterm))

(use-package smartparens
  :diminish
  smartparens-mode
  smartparens-strict-mode
  smartparens-global-mode
  smartparens-global-strict-mode

  :commands
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

  ;; Don't load dashboard if we are launching Emacs with a file argument
  :if
  (< (length command-line-args) 2)

  :diminish
  dashboard-mode

  :hook
  ((dashboard-mode . page-break-lines-mode))

  :config
  (dashboard-setup-startup-hook)

  :custom
  (dashboard-banner-logo-title "Welcome to Emacs!" "The title message.")
  (dashboard-startup-banner
   ;; Straight doesn't seem to like grabbing the banner from here
   (concat hgs-emacs-config-directory "data/banner.txt")
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
  (dashboard-projects-backend
   'project-el
   "Use project.el as a backend for project related stuff.")
  (dashboard-projects-switch-function
   #'project-switch-project
   "Which function to use for switching projects from the dashboard.")
  (dashboard-week-agenda t "Show upcoming seven days' agenda.")
  (dashboard-page-separator
   "\n\f\n"
   "Use form feed for `page-break-lines-mode' to show horizontal lines.")
  (initial-buffer-choice
   (lambda ()
     (get-buffer "*dashboard*"))
   "Show the dashboard as the initial buffer even for the Emacs client."))

(use-package reformatter
  :functions
  ;; Autoload this package when other modes try to define formatters
  reformatter-define)

(use-package undo-fu
  :defines
  hgs--undo-prefix-map

  :bind
  (:prefix "C-c u"
           :prefix-map hgs--undo-prefix-map
           :prefix-docstring "Undo commands"
           ("u" . undo-fu-only-undo)
           ("r" . undo-fu-only-redo)
           ("R" . undo-fu-only-redo-all)
           ("c" . undo-fu-disable-checkpoint))
  (:map global-map
        ([remap undo] . undo-fu-only-undo)
        ([remap undo-redo] . undo-fu-only-redo))

  :init
  (which-key-add-key-based-replacements
    "C-c u" "Undo")

  :custom
  (undo-fu-allow-undo-in-region
   nil "Don't allow undo-in-region.")
  (undo-fu-ignore-keyboard-quit
   nil "Use C-g for non-linear traversal behavior."))

(use-package undo-fu-session
  :diminish
  global-undo-fu-session-mode
  undo-fu-session-mode

  :commands
  global-undo-fu-session-mode
  undo-fu-session-save
  undo-fu-session-recover
  undo-fu-session-compression-update

  :hook
  ((prog-mode text-mode) . undo-fu-session-mode)

  :custom
  (undo-fu-session-incompatible-files
   '("/COMMIT_EDITMSG\\'" "git-rebase-todo\\'")
   "Regexes for files for which to not persist undo state.")
  (undo-fu-session-incompatible-major-modes
   nil "List of major modes for which to not persist undo state.")
  (undo-fu-session-linear nil "Persist a non-linear undo history.")
  (undo-fu-session-compression 'gz "Use gzip to compress session info.")
  (undo-fu-session-file-limit
   nil "Don't limit the number of persisted state files.")
  (undo-fu-session-directory
   (concat hgs-emacs-state-directory "undo-fu-session")
   "Put persisted undo state in the state directory."))

(use-package vundo
  :bind
  (:map hgs--undo-prefix-map
        ("v" . vundo)))

(use-package project
  :demand t

  :custom
  (project-list-file
   (concat hgs-emacs-state-directory "projects")
   "Where to cache known projects.")
  (project-switch-use-entire-map t "Use entire `project-prefix-map' as a basis
for project switch command dispatch."))

(use-package embark-consult
  :after
  embark
  consult

  ;; This package is needed by default to integrate embark actions with consult
  ;; commands properly (for example, consult-buffer's items won't have
  ;; appropriate actions without it). The hook/command prevents loading this
  ;; immediately, so we must demand it.
  :demand t

  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package json-mode
  :mode
  (("\\.json\\'" . json-mode)))

(use-package flycheck
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
        ("C-x M-g" . magit-dispatch))

  :custom
  (magit-bind-magit-project-status t "Add magit project status to the project.el
switch list."))

(use-package consult-flycheck
  :after
  consult
  flycheck

  :bind
  (:map flycheck-command-map
        ("!" . consult-flycheck)))

(use-package treemacs
  :commands
  treemacs-git-mode
  treemacs-fringe-indicator-mode

  :hook
  ((treemacs-mode) . treemacs-follow-mode)
  ((treemacs-mode) . treemacs-filewatch-mode)
  ((treemacs-mode) . hgs--treemacs-fringe-indicator-mode)
  ((treemacs-mode) . hgs--treemacs-git-mode)

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
   (concat hgs-emacs-cache-directory "treemacs-persist")
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

(use-package docker
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
  :demand t

  ;; N.B Having all-the-icons marked as a dependency causes this to refuse to
  ;; properly setup hooks
  ;;
  ;; :after
  ;; all-the-icons

  :init
  (defun hgs--enable-doom-modeline (&optional frame)
    (message "Enabling doom modeline!")
    (doom-modeline-mode +1))

  ;; We utilize frame creation hooks for force enabling the doom modeline
  ;; package
  :hook
  ((hgs-frame-customization) . hgs--enable-doom-modeline))

(use-package erc-hl-nicks
  :after
  erc)

(use-package message
  :defines
  hgs-smtp-routes

  :functions
  hgs--generate-smtp-routes
  hgs--generate-smtp-method-header
  hgs--gmail-from-addr-to-smtp-route-spec

  :init
  (defcustom hgs-smtp-routes '()
    "List of mappings of `From' headers to SMTP servers for outgoing messages."
    :group 'personal
    :type '(alist :key-type regexp :value-type function))

  (defun hgs--gmail-from-addr-to-smtp-route-spec (from-addr)
    "Maps a Gmail From address to an smtp route specification."
    `(:from-addr ,from-addr
                 :smtp-user ,from-addr
                 :smtp-server "smtp.gmail.com"
                 :smtp-port 587))

  (defun hgs--generate-smtp-routes (routing-table from-addr)
    "Maps a `From' address to an smtp route for use with `HGS-SMTP-ROUTES'.
This function relies on regex matching based on the `From' mail address"
    (let ((results
           (cl-loop for (regex . generator)
                    in routing-table
                    collect (when (string-match-p regex from-addr)
                              (funcall generator from-addr)))))
      (when (cl-every #'null results)
        (warn "Unable to generate matching route using your From address `%s'"
              from-addr))
      results))

  (defun hgs--generate-smtp-method-header (sender &optional routing-table)
    "Generate a `X-Message-SMTP-Method' header to SMTP route outgoing mail.
This function uses `HGS-SMTP-ROUTES' to perform the From: -> SMTP
server mappings if no mapping supplied via `ROUTING-TABLE'."
    (unless (or routing-table hgs-smtp-routes)
      (error "Unable to find non-empty routing table!"))
    (unless (> (length sender) 0)
      (error "Expected non-empty from address!"))
    (let* ((routes
            (cl-loop for route
                     in (hgs--generate-smtp-routes (or routing-table
                                                       hgs-smtp-routes)
                                                   sender)
                     when (not (null route))
                     collect (cl-destructuring-bind
                                 (&key from-addr smtp-server smtp-port
                                       smtp-user)
                                 route
                               (when (string-match from-addr sender)
                                 (format "smtp %s %d %s"
                                         smtp-server
                                         smtp-port
                                         smtp-user)))))
           (first-route (seq-find #'identity routes)))
      (unless first-route
        (warn "Could not find SMTP Server for this Sender address: %s.
You might want to add it to the SMTP routing table `HGS-SMTP-ROUTES'"
              sender))
      first-route))

  :custom
  (message-send-mail-function
   #'sendmail-query-once
   "Query for mail send function on first use (NOTE: Only queries if
`smtpmail-send-it' would require configuration).")
  (message-sendmail-envelope-from
   'header "Derive envelope from via outgoing mail headers for sendmail.")
  (message-directory
   (concat hgs-data-directory "mail") "Base directory for all core mail paths.")
  (message-auto-save-directory
   (concat hgs-emacs-state-directory "mail-drafts")
   "Where to place draft messages")
  (message-default-mail-headers
   "Cc: \n" "Add CC header to new mail by default.")
  (hgs-smtp-routes
   '((".*@gmail.com$" . hgs--gmail-from-addr-to-smtp-route-spec))
   "Alist of From address matching regex to smtp route spec generators."))

(use-package smtpmail
  :custom
  (smtpmail-local-domain
   (if (>= emacs-major-version 25)
       (car (split-string (shell-command-to-string "hostname -f")))
     nil)
   "SMTP servers expect FQDN, but `SYSTEM-NAME' (the default) returns short
hostname after emacs 25."))

(use-package mail-source
  :custom
  (mail-source-directory
   (concat hgs-data-directory "mail")
   "We don't use this, but point it at a sensible mail directory anyway."))

(use-package sendmail
  :custom
  (mail-default-directory
   (concat hgs-emacs-state-directory "mail")
   "Put mail auto-saves in the state directory")
  (mail-specify-envelope-from t "Specify envelope from to the specified MUA.")
  (mail-envelope-from
   'header "Derive envelope from via outgoing mail headers."))

;; Mail templating
(use-package message-templ
  :defines
  message-templ-alist
  message-templ-config-alist

  :functions
  message-templ-apply

  :bind
  (:map message-mode-map
        ("C-c s" . message-templ-select))

  :custom
  (message-templ-alist
   '() "Define empty defaults for template contents.")
  (message-templ-config-alist
   '() "Define empty defaults for mapping mail headers to templates."))

(use-package notmuch
  :init
  (require 'cl-lib)
  (require 'rx)
  (defun hgs--notmuch-list-profiles-by-filesystem (&optional config-directory)
    "Infers list of profiles using directories available in `CONFIG-DIRECTORY'.
If no `CONFIG-DIRECTORY' is provided, a reasonable default is
used. `CONFIG-DIRECTORY' is expected to point to your XDG
structured Notmuch configuration directory."
      (let* ((notmuch-config-directory
              (or config-directory
                  (concat hgs-user-directory ".config/notmuch")))
             (special-directory-regex
              "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)")
             (additional-exclusions-regex
              '("default")))
        (cl-flet* ((make-exclusion
                    (lambda (path)
                      (cl-some
                       (lambda (x)
                         (not
                          (string-match-p (rx bol (regex x) eol) path)))
                       additional-exclusions-regex)))
                   (to-profile-name
                    (lambda (path)
                      (file-name-base (directory-file-name path))))
                   (make-exclusions
                    (lambda (paths)
                      (seq-filter #'make-exclusion paths)))
                   (to-profile-names
                    (lambda (paths)
                      (seq-map #'to-profile-name paths)))
                   (only-directories
                    (lambda (paths)
                      (seq-filter (lambda (path)
                                    (file-directory-p path))
                                  paths))))

          (make-exclusions
           (to-profile-names
            (only-directories
             (directory-files notmuch-config-directory
                              'full
                              special-directory-regex
                              'nosort)))))))

  (defcustom hgs-notmuch-list-profiles
    #'hgs--notmuch-list-profiles-by-filesystem
    "Function used for generating a list of available Notmuch profiles."
    :type 'function
    :group 'personal)

  (defun hgs--notmuch-show-view-as-patch ()
    "View the the current message as a patch via `DIFF-MODE'."
    (interactive)
    (require 'notmuch-lib)
    (let* ((id (notmuch-show-get-message-id))
           (msg (notmuch-show-get-message-properties))
           (part (notmuch-show-get-part-properties))
           (subject (concat "Subject: " (notmuch-show-get-subject) "\n"))
           (diff-default-read-only t)
           (buf (get-buffer-create (concat "*notmuch-patch-" id "*")))
           (map (make-sparse-keymap)))
      (define-key map "q" 'notmuch-bury-or-kill-this-buffer)
      (switch-to-buffer buf)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert subject)
        (insert (notmuch-get-bodypart-text msg part nil)))
      (set-buffer-modified-p nil)
      (diff-mode)
      (let ((new-ro-bind (cons 'buffer-read-only map)))
        (add-to-list 'minor-mode-overriding-map-alist new-ro-bind))
      (goto-char (point-min))))

  :bind
  (:map notmuch-hello-mode-map
        ("C-c M-p" . hgs-notmuch-change-profile))
  (:map notmuch-show-part-map
        ("d" . hgs--notmuch-show-view-as-patch))

  :config
  (defun hgs-notmuch-change-profile (&optional)
    "Select from available Notmuch profiles to use."
    (interactive)
    (declare-function hgs-notmuch-list-profiles "core-config")
    (let ((profile-env-var "NOTMUCH_PROFILE")
          (new-profile
           (completing-read "Select Notmuch profile: "
                            (funcall hgs-notmuch-list-profiles)
                            nil nil)))
      (setenv profile-env-var new-profile)))

  :custom
  (notmuch-address-command
   'internal "Use `notmuch address' for address completion.")
  (notmuch-address-save-filename
   (concat hgs-emacs-cache-directory "notmuch-addresses")
   "Cache file for notmuch completed addresses.")
  (notmuch-crypto-process-mime
   t "Read & verify encrypted/signed mime messages.")
  (notmuch-saved-searches
   '((:name "inbox"
            :query "tag:inbox"
            :key "i")
     (:name "unread"
            :query "tag:unread"
            :key "u")
     (:name "flagged"
            :query "tag:flagged"
            :key "f")
     (:name "sent"
            :query "tag:sent"
            :key "t")
     (:name "drafts"
            :query "tag:draft"
            :key "d")
     (:name "mail list"
            :query "tag:mail-list"
            :key "m")
     (:name "all mail"
            :query "not (tag:draft or tag:sent or tag:trash or tag:spam)"
            :key "a"))
   "Saved searches accessible via jump table."))

;; Simple tabulated list UI package for different types of daemons on a linux
;; system
(use-package daemons
  :commands
  daemons

  :custom
  (daemons-always-sudo nil "Don't always sudo on daemon commands")
  ;; This is a poor interface really. We have to change the variable to enter
  ;; system mode
  (daemons-systemd-is-user t "Run systemd commands in user mode"))

(provide 'core-config)

;; Local Variables:
;; mode: emacs-lisp
;; coding: utf-8-unix
;; fill-column: 80
;; require-final-newline: t
;; End:

;;; core-config.el ends here
