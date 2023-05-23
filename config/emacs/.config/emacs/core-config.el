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
  :ensure nil
  :demand t

  :preface
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

  (defun hgs--suspend-frame ()
    "In a GUI environment, do nothing; otherwise `suspend-frame'."
    (interactive)
    (if (display-graphic-p)
        (message "suspend-frame disabled for graphical displays.")
      (suspend-frame)))

  :bind
  (:map global-map
        ;; Prevent us from accidentally suspending frame in non-graphical
        ;; displays.
        ([remap suspend-frame] ("No-op" . hgs--suspend-frame))

        ;; Replace the case altering bindings with their dwim variants to make
        ;; life easier for free!
        ([remap upcase-word] ("Upcase" . upcase-dwim))
        ([remap downcase-word] ("Downcase" . downcase-dwim))
        ([remap capitalize-word] ("Capitalize" . capitalize-dwim)))

  :init
  (add-hook 'hgs-frame-customization-hook #'hgs--setup-default-fonts -50)
  (add-hook 'hgs-frame-customization-gui-hook #'hgs--setup-default-gui -50)
  (add-hook 'hgs-frame-customization-tui-hook #'hgs--setup-default-tui -50)

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

  ;; Indicate the depth of recursion of the mini-buffer in the mode-line
  (minibuffer-depth-indicate-mode +1)

  :custom
  (require-final-newline t)
  (completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (case-fold-search t)
  (use-dialog-box nil)
  (kill-whole-line nil)
  (cursor-type 'bar)
  (enable-recursive-minibuffers t)
  (visible-bell nil)
  (fill-column 80)
  (indent-tabs-mode nil)
  (tab-width 2)
  (inhibit-startup-message t)
  (inhibit-startup-echo-area-message nil)
  (inhibit-default-init t)
  (initial-scratch-message nil)
  (auto-save-list-file-prefix
   (concat hgs-emacs-state-directory "auto-save/sessions/"))
  (sentence-end-double-space nil)
  ;; Tell Emacs that we primarily use left-to-right languages
  (bidi-paragraph-direction 'left-to-right)
  (ring-bell-function #'ignore)
  (history-delete-duplicates t)
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
  :ensure nil
  :defer t

  :bind
  ;; Allows us to use a narrowing framework for capf completions, which in my
  ;; opinion is better UX than company-mode.
  (:map global-map
        ("C-c ;" ("Completion-at-point" . completion-at-point))))

(use-package gamegrid
  :ensure nil
  :defer t

  :config
  (setq gamegrid-user-score-file-directory
        (concat hgs-emacs-state-directory "games/gamegrid")))

(use-package timeclock
  :ensure nil
  :defer t

  :custom
  (timeclock-file (concat hgs-emacs-data-directory "timeclock/log")))

(use-package help
  :ensure nil
  :defer t

  :hook
  (help-mode . turn-on-visual-line-mode)

  :custom
  (help-window-select 'always))

(use-package compile
  :ensure nil
  :defer t

  :preface
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

  :config
  (add-hook 'compilation-filter-hook #'hgs--filter-compilation-buffer)
  ;; Make setting compile command safe via local variables
  (put 'compile-command 'safe-local-variable #'stringp)

  :custom
  (compilation-scroll-output 'first-error)
  (compilation-skip-threshold 2 "Don't stop scrolling unless hit an error.")
  (compilation-ask-about-save t))

(use-package gud
  :ensure nil
  :defer t)

(use-package pulse
  :ensure nil
  :defer t

  :preface
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

  :init
  (hgs--add-pulse-advice)

  :custom
  (pulse-delay 0.03 "Delay between pulse iterations (seconds).")
  (pulse-iterations 10 "Number of iterations per pulse."))

(use-package find-file
  :ensure nil
  :defer t

  :custom
  (ff-case-fold-search t)
  (ff-always-in-other-window nil)
  (ff-ignore-include nil)
  (ff-always-try-to-create nil)
  (ff-quiet-mode t "Don't trace searched directories."))

(use-package ediff
  :ensure nil
  :defer t

  :config
  ;; Attempt to undo the window configuration change when exiting an Ediff
  ;; session.
  (add-hook 'ediff-after-quit-hook-internal #'winner-undo)

  :custom
  (ediff-keep-variants nil "Kill Ediff buffers on exit by default.")
  (ediff-window-setup-function
   #'ediff-setup-windows-plain
   "Use a single frame for ediff.")
  (ediff-split-window-function #'split-window-vertically))

(use-package xt-mouse
  :ensure nil
  :defer t
  :unless (display-graphic-p)

  :diminish xterm-mouse-mode

  :hook
  ;; Enable xterm mouse mode by default if running in the terminal
  ((prog-mode text-mode special-mode) . xterm-mouse-mode))

(use-package delsel
  :ensure nil
  :defer t

  :diminish delete-selection-mode

  :hook
  ;; Active regions should be deleted by normal deletion commands like expected
  ;; in all modes
  ((prog-mode text-mode) . delete-selection-mode))

(use-package hl-line
  :ensure nil
  :defer t
  ;; >27 is required for fast line highlight
  :unless (version< emacs-version "27")

  :diminish hl-line-mode

  :hook
  ((prog-mode text-mode) . hl-line-mode))

(use-package tab-bar
  :ensure nil
  :defer t
  :unless (version<= emacs-version "27.1.0")

  :diminish tab-bar-mode

  :hook
  ((prog-mode text-mode special-mode) . tab-bar-mode)

  :custom
  (tab-bar-show nil "Rely on `tab-switch' for searching tabs.")
  (tab-bar-new-tab-choice t "Start new tab on current buffer."))

(use-package simple
  :ensure nil
  :defer t

  :diminish
  line-number-mode
  column-number-mode
  auto-fill-mode

  :hook
  ((prog-mode text-mode) . line-number-mode)
  ((prog-mode text-mode) . column-number-mode)
  ((text-mode) . turn-on-auto-fill))

;; Slow & old
(use-package linum
  :ensure nil
  :defer t
  :when (version< emacs-version "26.0.50")

  :diminish
  global-linum-mode
  linum-mode

  :hook
  ((prog-mode text-mode) . linum-mode))

;; Fast & shiny
(use-package display-line-numbers
  :ensure nil
  :defer t

  :unless (version<= emacs-version "26.0.50")

  :diminish
  global-display-line-numbers-mode
  display-line-numbers-mode

  :hook
  ((prog-mode text-mode) . display-line-numbers-mode))

(use-package so-long
  :ensure nil
  :defer t
  :unless (version< emacs-version "27.1")

  :diminish
  global-so-long-mode
  so-long-mode

  :hook
  ;; so-long will automatically figure out when to activate the workhorse minor
  ;; mode with the global mode active.
  ((prog-mode text-mode) . global-so-long-mode)

  :custom
  (so-long-action #'so-long-minor-mode)
  (so-long-threshold 1000 "Threshold for what length is considered long.")
  (so-long-max-lines 100 "Number of non-blank lines to test for length."))

(use-package whitespace
  :ensure nil
  :defer t

  :diminish
  global-whitespace-mode
  whitespace-mode

  :hook
  ((prog-mode text-mode) . whitespace-mode)

  :config
  (put 'whitespace-line-column 'safe-local-variable #'integerp)

  :custom
  (whitespace-line-column
   nil
   "Max line length for whitespace mode. Uses `fill-column' when set to nil.")
  (whitespace-style
   '(face
     trailing
     lines-tail
     space-after-tab
     space-before-tab)
   "What whitespace should we visualize?"))

(use-package apropos
  :ensure nil
  :defer t

  :custom
  (apropos-do-all t "Activate extensive search by default."))

(use-package files
  :ensure nil
  :defer t

  :custom
  (tags-revert-without-query t)
  (large-file-warning-threshold (* 1024 1024 200))
  (auto-save-file-name-transforms
   `((".*" ,(concat hgs-emacs-state-directory) "backup")
     t))
  (backup-directory-alist
   `((".*" . ,(concat hgs-emacs-state-directory "backup"))))
  (backup-by-copying t "Always copy rather than symlink for backups."))

(use-package subword
  :ensure nil
  :defer t

  :diminish
  global-subword-mode
  subword-mode

  :hook
  ((prog-mode text-mode special-mode minibuffer-setup) . subword-mode))

(use-package recentf
  :ensure nil
  :defer t

  :diminish
  recentf-mode

  :hook
  ((prog-mode text-mode special-mode) . recentf-mode)

  :custom
  (recentf-save-file (concat hgs-emacs-state-directory "recentf/save.el"))
  (recentf-max-menu-items 25)
  (recentf-max-saved-items 25))

(use-package abbrev
  :ensure nil
  :defer t

  :diminish
  abbrev-mode

  :custom
  (abbrev-file-name (concat hgs-emacs-config-directory "abbrev/default.el"))
  (abbrev-suggest nil)
  (abbrev-suggest-hint-threshold 3)
  (abbrev-all-caps nil "Don't reflect upper-case abbrevs in expansion."))

(use-package autoinsert
  :ensure nil
  :defer t

  :custom
  (auto-insert-directory (concat hgs-emacs-config-directory "autoinsert/")))

(use-package vc
  :ensure nil
  :defer t

  :bind-keymap
  ("C-x v" . vc-prefix-map)

  :bind
  (:map vc-prefix-map
        ("=" . ediff-revision))

  :custom
  (vc-follow-symlinks t))

(use-package eudc-vars
  :ensure nil
  :defer t

  :custom
  (eudc-options-file (concat hgs-emacs-config-directory "eudc/options.el")))

(use-package remember
  :ensure nil
  :defer t

  :custom
  (remember-data-file (concat hgs-emacs-data-directory "remember/notes"))
  (remember-data-directory (concat hgs-emacs-data-directory "remember/")))

(use-package ido
  :ensure nil
  :defer t

  :diminish
  ido-mode

  :custom
  (ido-save-directory-list-file
   (concat hgs-emacs-state-directory "ido/save-directory-list.el")))

(use-package saveplace
  :ensure nil
  :defer t

  :diminish
  save-place-mode

  :custom
  (save-place-file (concat hgs-emacs-state-directory "saveplace/persist.el")))

(use-package gnus
  :ensure nil
  :defer t

  :custom
  (gnus-init-file (concat hgs-emacs-config-directory "gnus/init.el"))
  (gnus-startup-file
   (concat hgs-emacs-config-directory "gnus/newsrc")
   "GNUS hardcodes newsrc.eld to be derived from this.")
  (gnus-dribble-directory (concat hgs-emacs-state-directory "gnus/dribble/")))

(use-package newsticker
  :ensure nil
  :defer t

  :config
  (make-directory newsticker-dir t)
  (make-directory (file-name-directory newsticker-cache-filename) t)

  :custom
  (newsticker-dir (concat hgs-emacs-state-directory "newsticker/"))
  (newsticker-cache-filename
   (concat hgs-emacs-cache-directory "newsticker/cache.el")))

(use-package dired
  :ensure nil
  :defer t

  :preface
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

  :bind
  (:map global-map
        ("C-x d" . dired))
  (:map dired-mode-map
        ("^" . hgs--dired-up-directory-clean))

  :config
  ;; Emacs disables this by default, but it's the only real way to use dired w/o
  ;; buffer spam.
  (put 'dired-find-alternate-file 'disabled nil)

  :custom
  (dired-listing-switches "-alh1v --group-directories-first")
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (dired-dwim-target t "Auto infer cross-window operations."))

(use-package image-dired
  :ensure nil
  :defer t
  :after
  (:all dired)

  :custom
  (image-dired-dir (concat hgs-emacs-cache-directory "image-dired/"))
  (image-dired-gallery-dir
   (concat hgs-emacs-state-directory "image-dired/gallery/"))
  (image-dired-db-file (concat hgs-emacs-state-directory "image-dired/db.el"))
  (image-dired-temp-image-file
   (concat hgs-emacs-cache-directory "image-dired/temp-image"))
  (image-dired-temp-rotate-image-file
   (concat hgs-emacs-cache-directory "image-dired/temp-rotate-image")))

(use-package calc
  :ensure nil
  :defer t

  :custom
  (calc-settings-file (concat hgs-emacs-config-directory "calc/settings.el")))

(use-package winner
  :ensure nil
  :defer t

  :diminish
  winner-mode

  :bind
  (:map winner-mode-map
        ("C-c <left>" ("Window undo" . winner-undo))
        ("C-c <right>" ("Window redo" . winner-redo)))

  :hook
  ((prog-mode text-mode special-mode) . winner-mode))

(use-package filesets
  :ensure nil
  :defer t

  :custom
  (filesets-menu-cache-file
   (concat hgs-emacs-cache-directory "filesets/menu.el")))

(use-package kkc
  :ensure nil
  :defer t

  :custom
  (kkc-init-file-flag (concat hgs-emacs-config-directory "kkc/init.el")))

(use-package desktop
  :ensure nil
  :defer t

  :init
  (setq desktop-dirname (concat hgs-emacs-state-directory "desktop/"))

  :config
  (cl-dolist (path desktop-path)
    (make-directory path t))

  :custom
  (desktop-path (list desktop-dirname)))

(use-package calendar
  :ensure nil
  :defer t

  :custom
  (diary-file (concat hgs-emacs-data-directory "diary/default")))

(use-package ecomplete
  :ensure nil
  :defer t

  :custom
  (ecomplete-database-file
   (concat hgs-emacs-state-directory "ecomplete/database.el")))

(use-package ede/base
  :ensure nil
  :defer t

  :custom
  (ede-project-placeholder-cache-file
   (concat hgs-emacs-cache-directory "ede/project-cache.el")))

(use-package srecode
  :ensure nil
  :defer t

  :custom
  (srecode-map-save-file (concat hgs-emacs-state-directory "srecode/map.el")))

(use-package semantic
  :ensure nil
  :defer t

  :custom
  (semanticdb-default-save-directory
   (concat hgs-emacs-cache-directory "semanticdb/cache")))

(use-package shadowfile
  :ensure nil
  :defer t

  :custom
  (shadow-info-file (concat hgs-emacs-data-directory "shadow/info.el"))
  (shadow-todo-file (concat hgs-emacs-data-directory "shadow/todo.el")))

(use-package savehist
  :ensure nil
  :defer t

  :diminish
  savehist-mode

  :hook
  (after-init . savehist-mode)

  :custom
  (savehist-file (concat hgs-emacs-cache-directory "savehist/history")))

(use-package eww
  :ensure nil
  :defer t

  :bind
  (:map search-map
        ("M-w" . eww-search-words))

  :config
  (make-directory eww-bookmarks-directory t)

  :custom
  (eww-bookmarks-directory (concat hgs-emacs-state-directory "eww/")))

(use-package type-break
  :ensure nil
  :defer t

  :custom
  (type-break-file-name
   (concat hgs-emacs-state-directory "type-break/state.el")))

(use-package tramp
  :ensure nil
  :defer t

  :custom
  (tramp-auto-save-directory
   (concat hgs-emacs-state-directory "tramp/auto-save"))
  (tramp-backup-directory-alist
   backup-directory-alist
   "Put Tramp backups in the same place as local backups.")
  (tramp-persistency-file-name
   (concat hgs-emacs-cache-directory "tramp/persistency.el"))
  (tramp-histfile-override
   nil
   "We don't want to store remote shell history locally."))

(use-package eshell
  :ensure t
  :defer t

  :autoload
  eshell/pwd

  :preface
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
  (eshell-directory-name
   (concat hgs-emacs-state-directory "eshell")
   "Use state directory for storing transient files (e.g. history etc.)")
  (eshell-rc-script (concat hgs-emacs-config-directory "eshell/rc"))
  (eshell-login-script (concat hgs-emacs-config-directory "eshell/login"))
  (eshell-aliases-file (concat hgs-emacs-config-directory "eshell/aliases"))
  (eshell-buffer-maximum-lines 20000)
  (eshell-highlight-prompt t)
  (eshell-hist-ignoredups t)
  (eshell-history-size 5000)
  (eshell-plain-echo-behavior t "Make `echo' imitate shell echo.")
  (eshell-prompt-function #'hgs--eshell-prompt-function)
  (eshell-prompt-regexp ".+^λ " "Tell Emacs how to find prompts in the buffer.")
  (eshell-scroll-to-bottom-on-input 'this)
  (eshell-scroll-to-bottom-on-output nil))

(use-package flyspell
  :ensure nil
  :defer t
  :when (executable-find "aspell")

  :diminish
  flyspell-mode
  flyspell-prog-mode

  :hook
  (text-mode . flyspell-mode)
  (prog-mode . flyspell-prog-mode)

  :custom
  (ispell-program-name "aspell")
  (ispell-extra-args
   '("--sug-mode=ultra" "--lang=en_US")))

(use-package epa
  :ensure nil
  :defer t

  :preface
  (defun hgs--epa-inhibit-backups ()
    "Inhibit backups when operating on encrypted files."
    (when (and buffer-file-name
               (string-match epa-file-name-regexp buffer-file-name))
        (message "Backup inhibited for this file `%s'." buffer-file-name)
        (setq-local backup-inhibited t)))

  :hook
  (find-file . hgs--epa-inhibit-backups)

  :custom
  (epa-file-inhibit-auto-save t))

(use-package erc
  :ensure nil
  :defer t

  :autoload
  hgs--erc-save-buffers-to-logs

  :functions
  hgs-erc-current-network-name

  :preface
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

  :hook
  (erc-mode . hgs--erc-disable-whitespace-mode)
  ;; Default to readonly when joining a channel to prevent fat fingering by
  ;; default
  (erc-join . read-only-mode)

  :config
  (add-to-list 'erc-modules 'notifications) ;; Enable notifications
  (add-to-list 'erc-modules 'spelling) ;; Enable spelling corrections
  (erc-update-modules)

  (defun hgs--erc-save-buffers-to-logs (&optional _)
    "Auto saves erc buffers to their log files when exiting emacs."
    (save-some-buffers t (lambda (&optional _)
                           (when (eq major-mode 'erc-mode)
                             t))))
  (advice-add #'save-buffers-kill-emacs :before #'hgs--erc-save-buffers-to-logs)

  (defun hgs-erc-current-network-name (&optional buffer-to-check)
    "Returns network name (not necessarily server).
Will use the current buffer unless `BUFFER-TO-CHECK' is non-nil,
in which case it will be used."
    (let ((channel-buffer (or buffer-to-check (current-buffer))))
      (with-current-buffer channel-buffer
        (or (and (fboundp 'erc-network-name) (erc-network-name))
            (erc-shorten-server-name
             (or erc-server-announced-name
                 erc-session-server))))))

  :custom
  (erc-nick user-login-name)
  (erc-user-full-name erc-nick)
  (erc-email-userid erc-nick)
  (erc-nick-uniquifier "_")
  (erc-join-buffer
   'bury
   "Don't bring channel buffers to the forefront when they appear.")
  (erc-hide-list '())
  (erc-lurker-hide-list
   '("JOIN"
     "PART"
     "QUIT"))
  (erc-lurker-threshold-time
   (* 60 (* 60 (* 24)))
   "Class anyone inactive for 24 hours as a lurker.")
  (erc-debug-log-file (concat hgs-emacs-state-directory "erc/debug.log"))
  (erc-send-whitespace-lines nil)
  (erc-rename-buffers
   t
   "Rename server buffers with network name instead of
server:port where possible. This is particularly useful for
bouncers, where you'll have multiple server buffers for different
networks, but they'll appear as the same server to the client."))

;; Manages joining channels (both manually and automatically)
(use-package erc-join
  :ensure nil
  :defer t

  :after
  (:all erc)

  :hook
  (erc-mode . erc-autojoin-enable)

  :custom
  (erc-autojoin-channels-alist '())
  (erc-autojoin-timing
   'ident
   "Auto-join after successful identification with NickServ."))

(use-package erc-fill
  :ensure nil
  :defer t

  :after
  (:all erc)

  :hook
  (erc-mode . erc-fill-enable)

  :custom
  (erc-fill-column fill-column)
  (erc-fill-function 'erc-fill-variable)
  (erc-fill-prefix
   (make-string (erc-timestamp-offset) ? )
   "Prefix wrapping with enough spaces to be just past the timestamp
(aligning with nicks)."))

;; Highlights or hides messages matching certain patterns
(use-package erc-match
  :ensure nil
  :defer t

  :after
  (:all erc)

  :hook
  (erc-mode . erc-match-enable)

  :custom
  (erc-pals '() "Pals to highlight.")
  (erc-fools '() "Fools to ignore.")
  (erc-keywords '() "Keywords to track."))

;; Tracks active erc buffers
(use-package erc-track
  :ensure nil
  :defer t

  :after
  (:all erc)

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
  (erc-track-exclude '())
  (erc-track-exclude-types
   '("JOIN"
     "NICK"
     "QUIT"
     "MODE"
     "AWAY"))
  (erc-track-exclude-server-buffer t))

;; Stores previous commands/text in a ring available for recall via M-p/M-n
(use-package erc-ring
  :ensure nil
  :defer t

  :after
  (:all erc)

  :hook
  (erc-mode . erc-ring-enable))

;; Hides mode changes from the servers
(use-package erc-netsplit
  :ensure nil
  :defer t

  :after
  (:all erc)

  :hook
  (erc-mode . erc-netsplit-enable))

;; Performs logging of channels
(use-package erc-log
  :ensure nil
  :defer t

  :after
  (:all erc)

  :preface
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

  :hook
  (erc-mode . erc-log-enable)

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
  :ensure nil
  :defer t

  :after
  (:all erc)

  :hook
  (erc-mode . erc-stamp-enable)

  :custom
  (erc-hide-timestamps nil)
  (erc-timestamp-only-if-changed-flag nil)
  (erc-insert-timestamp-function #'erc-insert-timestamp-left)
  (erc-timestamp-format "[%H:%M:%S]"))

;; Keeps the erc buffers to a manageable size
(use-package erc-truncate
  :ensure nil
  :defer t

  :after
  (:all erc)

  :hook
  (erc-mode . erc-truncate-enable)

  :custom
  (erc-max-buffer-size 30000)
  (erc-truncate-buffer-on-save nil))

(use-package erc-backend
  :ensure nil
  :defer t

  :after
  (:all erc)

  :custom
  (erc-server-reconnect-attempts 5)
  (erc-server-reconnect-timeout 3))

(use-package erc-services
  :ensure nil
  :defer t

  :after
  (:all erc)

  :hook
  (erc-mode . erc-services-enable)

  :custom
  (erc-prompt-for-password nil)
  (erc-prompt-for-nickserv-password nil))

(use-package erc-dcc
  :ensure nil
  :defer t

  :after
  (:all erc)

  :config
  (make-directory erc-dcc-get-default-directory t)

  :custom
  (erc-dcc-get-default-directory
   (concat hgs-emacs-data-directory "erc/dcc/")))

(use-package erc-hl-nicks
  :ensure nil
  :defer t

  :after
  (:all erc))

(use-package url
  :ensure nil
  :defer t

  :custom
  (url-cache-directory (concat hgs-emacs-cache-directory "url"))
  (url-configuration-directory (concat hgs-emacs-data-directory "url"))
  (url-cookie-file (concat hgs-emacs-state-directory "url/cookies.el"))
  (url-history-file (concat hgs-emacs-state-directory "url/history.el")))

(use-package quickurl
  :ensure nil
  :defer t

  :custom
  (quickurl-url-file (concat hgs-emacs-state-directory "quickurl/urls")))

;; Network Security Manager -- Manages TLS certs
(use-package nsm
  :ensure nil
  :defer t

  :custom
  (nsm-settings-file (concat hgs-emacs-state-directory "network-security/data")))

(use-package bookmark
  :ensure nil
  :defer t

  :custom
  (bookmark-default-file
   (concat hgs-emacs-state-directory "bookmark/defaults.el")))

(use-package custom
  :ensure nil
  :defer t

  :custom
  (custom-theme-directory (concat hgs-emacs-config-directory "themes")))

(use-package python
  :ensure nil
  :defer t

  :defines
  python-indent-guess-indent-offset

  :custom
  (python-indent-offset 2)
  (python-indent-guess-indent-offset t))

;; C/C++
(use-package cc-mode
  :ensure nil
  :defer t

  :preface
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
  :ensure nil
  :defer t

  :bind
  (:prefix "C-c o"
           :prefix-map hgs--org-prefix-map
           :prefix-docstring "Org commands"
           ("l" ("Store link" . org-store-link))
           ("c" ("Capture" . org-capture))
           ("a" ("Agenda" . org-agenda))
           ("j" ("Go to clock" . org-clock-goto)))

  :init
  (which-key-add-key-based-replacements
    "C-c o" "Org")

  :custom
  (org-directory hgs-org-directory)
  (org-clock-persist-file
   (concat hgs-emacs-state-directory "org/clock-persist.el"))
  (org-id-locations-file
   (concat hgs-emacs-state-directory "org/id-locations.el"))
  (org-publish-timestamp-directory
   (concat hgs-emacs-state-directory "org/timestamps/"))
  (org-default-notes-file (concat hgs-org-directory "notes.org"))
  (org-agenda-files `(,hgs-org-directory))
  (org-archive-location
   (concat hgs-org-directory "archive/%s::datetree/")
   "Place archivals into an organized datetree in an archive sub-directory.")
  (org-refile-targets
   '((nil : maxlevel . 9)
     (org-agenda-files :maxlevel . 9))
   "Allow us to refile to all Org files in the agenda and current buffer to an
arbitrary depth.")
  (org-refile-use-outline-path
   'file
   "Show the file name as part of the outline path when refiling.")
  (org-refile-allow-creating-parent-nodes
   'confirm
   "Prompt when wanting to create new nodes during a refile.")
  (org-outline-path-complete-in-steps
   nil
   "Allow us to complete the path using a narrowing framework.")
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
  :ensure nil
  :defer t

  :after
  (:all org)

  :diminish
  org-indent-mode

  :custom
  (org-indent-indentation-per-level 2))

;; Personal lisp packages

;; HERE

;; Third-party package configuration

(use-package go-mode
  :ensure nil
  :defer t)

(use-package rustic-mode
  :ensure nil
  :defer t

  :custom
  (rustic-lsp-setup-p nil "Don't automatically try to setup lsp-mode")
  (rustic-format-trigger 'on-save "Format Rust code on save."))

(use-package vimrc-mode
  :ensure nil
  :defer t)

(use-package dockerfile-mode
  :ensure nil
  :defer t

  :config
  ;; Allow the use to put a file-local variable specifying the image name.
  (put 'docker-image-name 'safe-local-variable #'stringp))

(use-package protobuf-mode
  :ensure nil
  :defer t

  :mode
  ;; Built-in mode autoload only deals with *.proto
  ("\\.pb\\'" . protobuf-mode))

(use-package bazel
  :ensure nil
  :defer t)

(use-package meson-mode
  :ensure nil
  :defer t)

(use-package jq-mode
  :ensure nil
  :defer t

  :after
  (:all json-mode)

  :bind
  (:map json-mode-map
        ("C-c C-j" ("JQ (Interactive)" . jq-interactively))))

(use-package toml-mode
  :ensure nil
  :defer t)

;; Dim non-focused windows for clarity. This seems to cause massive terminal
;; slow-down when changing active window, so I might eventually remove it.
(use-package dimmer
  :ensure nil
  :demand t

  :diminish
  dimmer-mode

  :preface
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
  (dimmer-adjustment-mode :both)
  (dimmer-fraction 0.15 "Higher means greater dimming.")
  (dimmer-watch-frame-focus-events
   nil
   "Don't react to frame-wide focusing changes. Needed to avoid
flashing on mouse navigation."))

(use-package avy
  :ensure nil
  :defer t

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
  (avy-keys-alist
   '()
   "Alist of Avy commands to keys to use for prompts. Falls back to `avy-keys'")
  (avy-style 'at "Overlay display style.")
  (avy-styles-alist '() "Alist of Avy commands to overlay display styles.")
  (avy-background nil "Use a gray background during selection.")
  (avy-all-windows t "Scan all windows on the selected for selection.")
  (avy-case-fold-search t "Ignore case for search.")
  (avy-single-candidate-jump
  nil
  "Don't auto-jump on single candidates to allow applying actions.")
  (avy-highlight-first
   nil
   "Don't highlight the first decision character, only first non-terminating
decision characters.")
  (avy-timeout-seconds 0.5 "How long `*-timer' commands should wait.")
  (avy-orders-alist
   '((avy-goto-char . avy-order-closest))
   "Alist allowing for specifying commands to use fewer characters when closer
to point."))

(use-package page-break-lines
  :ensure nil
  :defer t

  :diminish
  page-break-lines-mode)

(use-package all-the-icons
  :ensure nil
  :demand t)

(use-package nerd-icons
  :ensure nil
  :demand t)

(use-package org-bullets
  :ensure nil
  :defer t

  :after
  (:all org)

  :diminish
  org-bullets-mode

  :hook
  (org-mode . org-bullets-mode)

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
  :ensure nil
  :defer t

  :after
  (:all org)

  :custom
  (org-mime-library 'mml))

(use-package exec-path-from-shell
  :ensure nil
  :demand t

  :preface
  (defun hgs--initialize-exec-path-from-shell ()
    "Perform an initial synchronization under some platforms."
    (when (memq window-system '(mac ns x))
      (exec-path-from-shell-initialize))
    (when (daemonp)
      (exec-path-from-shell-initialize)))

  :config
  (hgs--initialize-exec-path-from-shell)

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
  :ensure nil
  :defer t

  :custom
  (auth-sources
   `(;; PGP encryped authinfo format
     (:source ,(concat hgs-emacs-data-directory "authinfo.gpg")))
   "Setup my ordered list of preferred authentication sources for Emacs."))

(use-package auth-source-pass
  :ensure nil
  :defer t

  :when (or (executable-find "pass")
            (executable-find "gopass"))

  :after
  (:all auth-source)

  :autoload
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
  :ensure nil
  :defer t

  :when (or (executable-find "pass")
            (executable-find "gopass"))

  :after
  (:all auth-source-pass)

  :custom
  (password-store-executable
   (if (executable-find "gopass")
       "gopass"
     "pass")
   "Prefer gopass if it is available."))

(use-package pass
  :ensure nil
  :defer t
  :when (or (executable-find "pass")
            (executable-find "gopass"))

  :after
  (:all password-store))

(use-package vertico
  :ensure nil
  :demand t

  :diminish
  vertico-mode

  :preface
  (defun hgs--vertico-completion-in-region (&rest args)
    "Use `consult-completion-in-region' if Vertico and Consult are available.
Otherwise use the default `completion--in-region' function."
    (apply (if (and vertico-mode (fboundp #'consult-completion-in-region))
               #'consult-completion-in-region
             #'completion--in-region)
           args))

  :config
  (vertico-mode +1)

  :custom
  (completion-in-region-function #'hgs--vertico-completion-in-region)
  (vertico-scroll-margin 0)
  (vertico-resize t "Dynamically resize minibuffer.")
  (vertico-count 15)
  (vertico-cycle t))

(use-package orderless
  :ensure nil
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
   '(orderless-initialism orderless-literal orderless-regexp)))

(use-package consult
  :ensure nil
  :demand t

  :bind
  (:map global-map
        ([remap yank-pop] ("Yank pop" . consult-yank-pop))
        ("C-h M" ("Man" . consult-man))
        ("C-c H" ("History" . consult-history))
        ("M-s e" ("Isearch history" . consult-isearch-history))
        ("C-x K" ("Kmacro" . consult-kmacro))
        ("C-M-#" ("Register" . consult-register))
        ("C-x r M-\"" ("Register load" . consult-register-load))
        ("C-x r M-'" ("Register store" . consult-register-store))
        ([remap bookmark-jump] ("Bookmark jump" . consult-bookmark))
        ("C-x M-:" ("Complex command" . consult-complex-command))
        ("C-x b" ("Switch buffer" . consult-buffer))
        ("C-x 4 b" ("Switch buffer (other window)"
                    . consult-buffer-other-window))
        ("C-x 5 b" ("Switch buffer (other frame)"
                    . consult-buffer-other-frame)))
  (:map goto-map
        ("g" ("Goto line" . consult-goto-line))
        ("M-g" ("Goto line" . consult-goto-line))
        ("e" ("Compile error" . consult-compile-error))
        ("f" ("Flycheck" . consult-flycheck))
        ("F" ("Flymake" . consult-flymake))
        ("o" ("Outline" . consult-outline))
        ("m" ("Mark" . consult-mark))
        ("k" ("Global mark" . consult-global-mark))
        ("i" ("Imenu" . consult-imenu))
        ("I" ("Imenu (multi)" . consult-imenu-multi)))
  (:map search-map
        ("f" ("Find" . consult-find))
        ("L" ("Locate" . consult-locate))
        ("g" ("Grep" . consult-grep))
        ("G" ("Gitgrep " . consult-git-grep))
        ("r" ("Ripgrep " . consult-ripgrep))
        ("l" ("Line" . consult-line))
        ("L" ("Line (multi) " . consult-line-multi))
        ("o" ("Occur" . occur))
        ("m" ("Multi-occur " . multi-occur))
        ("k" ("Keep lines" . consult-keep-lines))
        ("u" ("Focus lines" . consult-focus-lines)))
  (:map isearch-mode-map
        ([remap isearch-edit-string]
         ("Isearch history" . consult-isearch-history))
        ("M-s l" ("Line" . consult-line))
        ("M-s L" ("Line (multi)" . consult-line-multi)))

  :config
  ;; Add thin lines, sorting and hide the mode line of the register preview
  ;; window
  (advice-add #'register-preview :override #'consult-register-window)

  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (register-preview-delay 0 "Set no delay for the register preview for speed.")
  (register-preview-function #'consult-register-format)
  (consult-narrow-key ">")
  (consult-widen-key "<")
  (consult-preview-key 'any "Trigger Consult previews with any key press.")
  (consult-project-root-function
   #'project-root
   "Use built-in project.el for finding the project root."))

(use-package marginalia
  :ensure nil
  :demand t

  :after
  (:all vertico)

  :diminish
  marginalia-mode

  :bind
  (:map global-map
        ("M-A" ("Marginalia cycle" . marginalia-cycle)))
  (:map minibuffer-local-map
        ("M-A" ("Marginalia cycle" . marginalia-cycle)))

  :config
  (marginalia-mode +1)

  :custom
  (marginalia-annotators
   '(marginalia-annotators-heavy
     marginalia-annotators-light
     nil)
   "Prefer richer, heavier annotations over lighter alternatives."))

(use-package embark
  :ensure nil
  :defer t

  :bind
  (:prefix "C-c e"
           :prefix-map hgs--embark-prefix-map
           :prefix-docstring "Embark commands"
           ;; You can access most fundamental embark operations such as
           ;; export/become/collect/live via act, so there's no need to rebind
           ;; them here.
           ;;
           ;; Contextual actions on object at point
           ("a" ("Act" . embark-act))
           ;; Do the default action to thing at point
           ("d" ("Dwim" . embark-dwim)))

  (:map global-map
   ;; Improved bindings help
   ([remap describe-bindings] ("Describe bindings" . embark-bindings)))
  (:map minibuffer-local-map
        ;; Convenience bindings for inside a minibuffer. My general thoughts are
        ;; that beyond act/become which are frequent and short operations one
        ;; shouldn't need shortcuts for embark in the minibuffer.
        ("C-;" ("Act" . embark-act))
        ("C-:" ("Dwim" . embark-dwim)))
  (:map embark-file-map
        ("j" ("Dired jump" . dired-jump)))

  :init
  (which-key-add-key-based-replacements
    "C-c e" "Embark")

  :custom
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
  :ensure nil
  :defer t

  :after
  (:all flyspell)

  :bind
  (:map global-map
        ("C-c $" ("Correction-at-point" . flyspell-correct-at-point)))

  :custom
  (flyspell-correct-highlight t "Highlight word being corrected."))

(use-package solarized-theme
  :ensure nil
  :demand t

  :preface
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

  :config
  (load-theme 'solarized-light 'no-confirm)

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

;; Handy package for colourizing following the code's inherent tree structure.
;; Extremely useful for structured content
(use-package prism
  :ensure nil
  :defer t)

(use-package async
  :ensure nil
  :defer t

  :init
  (setq async-byte-compile-log-file
        (concat hgs-emacs-state-directory "async/bytecomp.log"))

  :hook
  (after-init . async-bytecomp-package-mode)
  (dired-mode . dired-async-mode))

(use-package transient
  :ensure nil
  :defer t

  :custom
  (transient-levels-file
   (concat hgs-emacs-state-directory "transient/levels.el"))
  (transient-values-file
   (concat hgs-emacs-state-directory "transient/values.el"))
  (transient-history-file
   (concat hgs-emacs-state-directory "transient/history.el")))

(use-package pdf-tools
  :ensure nil
  :defer t

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
  (pdf-view-display-size 'fit-width))

(use-package with-editor
  :ensure nil
  :defer t

  :preface
  (defun hgs--with-editor-export-editor ()
    "Run `with-editor-export-editor' once for each possible editor variable."
    (dolist (var '("EDITOR"
                   "VISUAL"
                   "GIT_EDITOR"
                   "HG_EDITOR"))
      (with-editor-export-editor var)))

  :hook
  ((shell-mode term-exec eshell-mode vterm-mode)
   . hgs--with-editor-export-editor)

  :bind
  (:map global-map
        ([remap async-shell-command]
         ("Async shell command" . with-editor-async-shell-command))
        ([remap shell-command]
         ("Shell command" . with-editor-shell-command))))

;; Trim whitespace on touched lines only automatically when saving
(use-package ws-butler
  :ensure nil
  :defer t

  :diminish
  ws-butler-global-mode
  ws-butler-mode

  :hook
  ((text-mode prog-mode) . ws-butler-mode)

  :custom
  (ws-butler-keep-whitespace-before-point nil))

(use-package expand-region
  :ensure nil
  :defer t

  :bind
  (:map global-map
        ("C-=" ("Expand region" . er/expand-region))))

(use-package corfu
  :ensure nil
  :defer t

  :autoload
  corfu--extra

  :preface
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
        ("M-m" ("Transfer to minibuffer" . hgs-corfu-move-to-minibuffer)))

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
  (corfu-auto t)
  (corfu-auto-delay 0.5 "Delay for auto-complete in seconds.")
  (corfu-auto-prefix 3 "Character prefix length before auto-complete occurs.")
  (corfu-quit-no-match
   'separator
   "Quit eagerly if no match to avoid popup getting in the way.")
  (corfu-preselect-first nil)
  (corfu-echo-documentation t)
  (corfu-on-exact-match 'insert)
  (corfu-quit-at-boundary nil "Stay alive even if there is no match.")
  ;; Note: M-SPC is already hijacked under Gnome for some other purpose
  (corfu-separator ?\s "Use M-SPC as the separator.")
  (corfu-preview-current nil "Preview the currently selected candidate.")
  (corfu-scroll-margin 2))

;; This is a cool package for providing some good default CAPF backends and
;; converting company backend to CAPF compatible backends, but at the moment
;; we don't really need it. We load it anyway. May remove at a later date.
(use-package cape
  :disabled t
  :ensure nil
  :defer t)

(use-package tree-sitter
  :ensure nil
  :defer t
  :when hgs-has-dynamic-module-support

  :diminish
  global-tree-sitter-mode
  tree-sitter-mode
  tree-sitter-hl-mode

  :commands
  tree-sitter-hl-mode

  :hook
  ;; I would like to just use prog-mode and ignore the message it outputs when
  ;; there isn't a grammar found, but unfortunately that seems to hang
  ;; daemonized Emacs. We instead just swallow errors via a proxy function.
  ;; The alternative is to toggle these per language pair which is not the
  ;; most ergonomic at scale.
  ((prog-mode) . hgs--tree-sitter-modes)

  :preface
  (defun hgs--tree-sitter-modes (&optional arg)
    "Proxy to `tree-sitter-mode' & `tree-sitter-hl-mode' swallowing errors."
    (let ((debug-on-error nil))
      (with-demoted-errors "Silenced error: %s"
        (tree-sitter-mode arg)
        (tree-sitter-hl-mode arg)))))

(use-package tree-sitter-langs
    :ensure nil
    :defer t
    :when hgs-has-dynamic-module-support

    :after
    (:all tree-sitter))

(use-package yasnippet
  :ensure nil
  :defer t

  :diminish
  yas-global-mode
  yas-minor-mode

  :mode
  (("\\.yasnippet\\'" . snippet-mode)
   ("\\.yas\\'" . snippet-mode))

  :hook
  ((prog-mode text-mode) . yas-minor-mode)

  :bind-keymap
  ("C-c &" ("Yasnippet" . yas-keymap))

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
  :ensure nil
  :defer t

  :after
  (:all yasnippet)

  :config
  (yasnippet-snippets-initialize))

(use-package eglot
  :ensure nil
  :defer t

  :hook
  ;; Mostly we can get away with using manual invocation where needed
  ;; per-session
  ((c-mode c++-mode python-mode) . eglot-ensure)

  :bind
  (:map eglot-mode-map
        ;; None as of yet
        )

  :custom
  (eglot-autoreconnect
   3
   "Only attempt reconnect to LSP server if the previous connection lasted at
least N seconds.")
  (eglot-connect-timeout
   30
   "Time given before a connection should be considered timed-out.")
  (eglot-sync-connect
   3
   "Synchronously block UI for only up to N seconds on connect.")
  (eglot-events-buffer-size
   2000000
   "Maximum number of characters allowed in the Eglot events buffer.")
  (eglot-autoshutdown
   nil
   "Don't shutdown LSP servers automatically when there are no more using
buffers. Prefer `eglot-shutdown'.")
  (eglot-extend-to-xref
   nil
   "Don't transiently consider out-of-project files you jump to as part of the
current workspace.")
  (eglot-confirm-server-initiated-edits
   t
   "Ask for confirmation when the server tries to alter buffers.")
  (eglot-ignored-server-capabilities '()))

(use-package which-key
  :ensure nil
  :defer t

  :diminish
  which-key-mode

  :hook
  ((prog-mode text-mode special-mode) . which-key-mode)

  :config
  (which-key-setup-side-window-bottom)

  :custom
  (which-key-popup-type 'side-window "Use the minibuffer for the key display.")
  (which-key-side-window-slot 0)
  (which-key-side-window-location 'bottom)
  (which-key-sort-order 'which-key-key-order)
  (which-key-idle-delay 2.0 "How long to wait before offering a guide.")
  (which-key-max-description-length 27)
  (which-key-add-column-padding 0 "Left padding for key display.")
  (which-key-show-prefix 'bottom "Display currently typed prefix at bottom."))

(use-package transpose-frame
  :ensure nil
  :defer t

  :bind
  (:map global-map
        ("C-c <up>" ("Transpose windows" . transpose-frame))
        ("C-c <down>" ("Rotate windows" . rotate-frame-clockwise))))

(use-package rainbow-delimiters
  :ensure nil
  :defer t

  :diminish
  rainbow-delimiters-mode

  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package cmake-mode
  :ensure nil
  :defer t)

(use-package yaml-mode
  :ensure nil
  :defer t)

(use-package csv-mode
  :ensure nil
  :defer t)

(use-package lua-mode
  :ensure nil
  :defer t

  :custom
  (lua-indent-level 2))

(use-package markdown-mode
  :ensure nil
  :defer t

  :mode
  ;; We assume we want GFM flavor for readmes
  ("README\\.md\\'" . gfm-mode))


(use-package restclient
  :ensure nil
  :defer t)

(use-package string-inflection
  :ensure nil
  :defer t

  :autoload
  string-inflection-get-current-word
  string-inflection-upcase-function
  string-inflection-upcase-p
  string-inflection-capital-underscore-function
  string-inflection-capital-underscore-p
  string-inflection-underscore-function
  string-inflection-underscore-p
  string-inflection-camelcase-function
  string-inflection-camelcase-p
  string-inflection-pascal-case-function
  string-inflection-pascal-case-p
  string-inflection-kebab-case-function
  string-inflection-kebab-case-p

  :commands
  string-inflection-upcase
  string-inflection-capital-underscore
  string-inflection-underscore
  string-inflection-camelcase
  string-inflection-pascal-case
  string-inflection-kebab-case

  :preface
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
  :ensure nil
  :defer t

  :diminish
  editorconfig-mode

  :hook
  (prog-mode . editorconfig-mode))

(use-package wgrep
  :ensure nil
  :defer t

  :bind
  (:map grep-mode-map
        ("C-c C-p" ("Activate wgrep" . wgrep-change-to-wgrep)))

  :custom
  (wgrep-enable-key "\C-c\C-p" "Key to make the grep buffer `wgrep' editable.")
  (wgrep-auto-save-buffer
   nil
   "Don't save buffer automatically when `wgrep-finish-edit'.")
  (wgrep-change-readonly-file
   nil
   "Don't apply changes regardless of whether the buffer is read-only."))

(use-package vterm
  :ensure nil
  :defer t
  :when hgs-has-dynamic-module-support)

(use-package smartparens
  :ensure nil
  :defer t

  :diminish
  smartparens-mode
  smartparens-strict-mode
  smartparens-global-mode
  smartparens-global-strict-mode

  :autoload
  hgs-sp-wrap-pair-prompt

  :bind
  (:map smartparens-mode-map
        ("C-M-k" ("Kill sexp" . sp-kill-sexp))
        ("C-M-<backspace>" ("Backward kill sexp" . sp-backward-kill-sexp))
        ("C-M-f" ("Forward sexp" . sp-forward-sexp))
        ("C-M-b" ("Backward sexp" . sp-backward-sexp))
        ("C-M-n" ("Up sexp" . sp-up-sexp))
        ("C-M-d" ("Down sexp" . sp-down-sexp))
        ("C-M-u" ("Backward up sexp" . sp-backward-up-sexp))
        ("C-M-p" ("Backward down sexp" . sp-backward-down-sexp))
        ("C-M-w" ("Copy sexp" . sp-copy-sexp))
        ("C-M-s" ("Splice sexp" . sp-splice-sexp))
        ("C-M-r" ("Splice sexp (kill around)" . sp-splice-sexp-killing-around))
        ("C-)" ("Forward slurp sexp" . sp-forward-slurp-sexp))
        ("C-}" ("Forward barf sexp" . sp-forward-barf-sexp))
        ("C-(" ("Backward slurp sexp" . sp-backward-slurp-sexp))
        ("C-{" ("Backward barf sexp" . sp-backward-barf-sexp))
        ("C-M-)" ("Wrap pair" . hgs-sp-wrap-pair-prompt))
        ("C-M-(" ("Unwrap sexp" . sp-unwrap-sexp))
        ("M-S" ("Split sexp" . sp-split-sexp))
        ("M-J" ("Join sexp" . sp-join-sexp))
        ("C-M-t" ("Transpose sexp" . sp-transpose-sexp)))

  :config
  (require 'smartparens-config)
  (defun hgs-sp-wrap-pair-prompt (arg)
    "Prompt for a character `ARG' to wrap the selection with as a pair of
delimiters."
    (interactive "sEnter a pair character: \n")
    (sp-wrap-with-pair arg))

  :hook
  (prog-mode . smartparens-mode))

(use-package dashboard
  :ensure nil
  :defer t
  ;; Don't load dashboard if we are launching Emacs with a file argument
  :when (< (length command-line-args) 2)

  :diminish
  dashboard-mode

  :hook
  (dashboard-mode . page-break-lines-mode)

  :init
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
  :ensure nil
  :defer t)

(use-package undo-fu
  :ensure nil
  :defer t

  :bind
  (:prefix "C-c u"
           :prefix-map hgs--undo-prefix-map
           :prefix-docstring "Undo commands"
           ("u" ("Undo" . undo-fu-only-undo))
           ("r" ("Redo" . undo-fu-only-redo))
           ("R" ("Redo all" . undo-fu-only-redo-all))
           ("c" ("Disable Checkpoint" . undo-fu-disable-checkpoint)))
  (:map global-map
        ([remap undo] ("Undo" . undo-fu-only-undo))
        ([remap undo-redo] ("Redo" . undo-fu-only-redo)))

  :init
  (which-key-add-key-based-replacements
    "C-c u" "Undo")

  :custom
  (undo-fu-allow-undo-in-region nil)
  (undo-fu-ignore-keyboard-quit
   nil
   "Use C-g for non-linear traversal behavior."))

(use-package undo-fu-session
  :ensure nil
  :defer t

  :diminish
  global-undo-fu-session-mode
  undo-fu-session-mode

  :hook
  ((prog-mode text-mode) . undo-fu-session-mode)

  :custom
  (undo-fu-session-incompatible-files
   '("/COMMIT_EDITMSG\\'" "git-rebase-todo\\'")
   "Regexes for files for which to not persist undo state.")
  (undo-fu-session-incompatible-major-modes nil)
  (undo-fu-session-linear nil)
  (undo-fu-session-compression 'gz)
  (undo-fu-session-file-limit nil)
  (undo-fu-session-directory
   (concat hgs-emacs-state-directory "undo-fu/session")))

(use-package vundo
  :ensure nil
  :defer t

  :bind
  (:map hgs--undo-prefix-map
        ("v" ("Vundo" . vundo))))

(use-package project
  :ensure nil
  :defer t

  :autoload
  project-root

  :config
  (make-directory (file-name-directory project-list-file) t)

  :custom
  (project-list-file
   (concat hgs-emacs-state-directory "project/list.el"))
  (project-switch-use-entire-map
   t
   "Use entire `project-prefix-map' as a basis for project switch command
dispatch."))

(use-package embark-consult
  :ensure nil
  ;; This package is needed by default to integrate embark actions with consult
  ;; commands properly (for example, consult-buffer's items won't have
  ;; appropriate actions without it). The hook/command prevents loading this
  ;; immediately, so we must demand it.
  :demand t

  :after
  (:all embark consult)

  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package json-mode
  :ensure nil
  :defer t)

(use-package flycheck
  :ensure nil
  :defer t

  :diminish
  flycheck-mode

  :bind-keymap
  ("C-c !" . flycheck-keymap-prefix)

  :init
  (which-key-add-key-based-replacements
    "C-c !" "Flycheck")

  :hook
  (prog-mode . flycheck-mode))

(use-package magit
  :ensure nil
  :defer t

  :after
  (:all transient with-editor)

  :bind
  (:map global-map
        ("C-x g" ("Magit status" . magit-status))
        ("C-x M-g" ("Magit dispatch" . magit-dispatch)))

  :custom
  (magit-bind-magit-project-status
   t
   "Add magit project status to the project.el switch list."))

(use-package consult-flycheck
  :ensure nil
  :defer t

  :after
  (:all consult flycheck)

  :bind
  (:map flycheck-command-map
        ("!" ("Flycheck" . consult-flycheck))))

(use-package docker
  :ensure nil
  :defer t

  :bind
  (:map global-map
        ("C-c D" ("Docker" . docker)))

  :config
  (defun hgs-toggle-docker-as-root ()
    "Toggle whether to run Docker as root on/off."
    (interactive)
    (message "`docker-run-as-root' is now %s" docker-run-as-root)
    (if (null docker-run-as-root)
        (setq docker-run-as-root t)
      (setq docker-run-as-root nil))))

(use-package doom-modeline
  :ensure nil
  :defer t

  :after
  (:all nerd-icons)

  :preface
  (defun hgs--enable-doom-modeline (&optional frame)
    (message "Enabling doom modeline!")
    (doom-modeline-mode +1))

  :hook
  (hgs-frame-customization . hgs--enable-doom-modeline))

(use-package message
  :ensure nil
  :defer t

  :custom
  (message-fill-column 72 "Use sensible wrapping for plain-text emails.")
  (message-send-mail-function
   #'sendmail-query-once
   "Query for mail send function on first use (NOTE: Only queries if
`smtpmail-send-it' would require configuration).")
  (message-sendmail-envelope-from
   'header
   "Derive envelope from via outgoing mail headers for sendmail.")
  (message-directory (concat hgs-data-directory "mail"))
  (message-auto-save-directory
   (concat hgs-emacs-state-directory "message/drafts"))
  (message-default-mail-headers "Cc: \n"))

(use-package smtpmail
  :ensure nil
  :defer t

  :custom
  (smtpmail-local-domain
   (if (>= emacs-major-version 25)
       (car (split-string (shell-command-to-string "hostname -f")))
     nil)
   "SMTP servers expect FQDN, but `SYSTEM-NAME' (the default) returns short
hostname after emacs 25."))

(use-package mail-source
  :ensure nil
  :defer t

  :custom
  (mail-source-directory
   (concat hgs-data-directory "mail")
   "We don't use this, but point it at a sensible mail directory anyway."))

(use-package sendmail
  :ensure nil
  :defer t

  :custom
  (mail-default-directory
   (concat hgs-emacs-state-directory "sendmail/auto-save"))
  (mail-specify-envelope-from t "Specify envelope from to the specified MUA.")
  (mail-envelope-from
   'header
   "Derive envelope from via outgoing mail headers."))

;; Mail templating
(use-package message-templ
  :ensure nil
  :defer t

  :autoload
  message-templ-apply

  :bind
  (:map message-mode-map
        ("C-c s" ("Select template" . message-templ-select)))

  :custom
;;   Example of Multiple Account Handling:
;;   (message-templ-config-alist
;;    '(("^From:.*me@email.com.*"
;;       (lambda ()
;;         (message-templ-apply "me-smtp-header")))
;;      "Alist for automatically applying templates on mail based on a regexp.")
;;  (message-templ-alist
;;   '(("me-smtp-header"
;;      ;; Add the following headers
;;      ("From" . "My Name <me@email.com>")
;;      ("X-Message-SMTP-Method" . "smtp smtp.server.com 587 me@email.com")))
;;   "Alist for defining named mail templates.")
  (message-templ-alist
   '()
   "Define empty defaults for mail templates.")
  (message-templ-config-alist
   '()
   "Define empty defaults for automatically applying templates to mail."))

(use-package notmuch
  :ensure nil
  :defer t

  :preface
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

  :bind
  (:map notmuch-hello-mode-map
        ("C-c M-p" ("Change profile" . hgs-notmuch-change-profile)))
  (:map notmuch-show-part-map
        ("d" ("Show as patch" . hgs--notmuch-show-view-as-patch)))

  :config
  (defun hgs--notmuch-show-view-as-patch ()
    "View the the current message as a patch via `DIFF-MODE'."
    (interactive)
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

  (defun hgs-notmuch-change-profile (&optional)
    "Select from available Notmuch profiles to use."
    (interactive)
    (let ((profile-env-var "NOTMUCH_PROFILE")
          (new-profile
           (completing-read "Select Notmuch profile: "
                            (funcall hgs-notmuch-list-profiles)
                            nil nil)))
      (setenv profile-env-var new-profile)))

  :custom
  (notmuch-init-file
   (concat hgs-emacs-config-directory "notmuch/init")
   "The Notmuch elisp init variable must not have a suffix, but the real file
should.")
  (notmuch-address-command 'internal)
  (notmuch-address-save-filename
   (concat hgs-emacs-cache-directory "notmuch/addresses"))
  (notmuch-crypto-process-mime t)
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
            :key "a"))))

;; Simple tabulated list UI package for different types of daemons on a linux
;; system
(use-package daemons
  :ensure nil
  :defer t

  :custom
  (daemons-always-sudo nil "Don't always sudo on daemon commands")
  (daemons-systemd-is-user t "Run systemd commands in user mode"))

(provide 'core-config)

;; Local Variables:
;; mode: emacs-lisp
;; coding: utf-8-unix
;; fill-column: 80
;; require-final-newline: t
;; End:

;;; core-config.el ends here
