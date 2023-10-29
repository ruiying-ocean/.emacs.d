;;;; init.el --- Emacs configuration -*- lexical-binding: t -*-

;;; This file contains my customized configuration codes, which
;;; are divided into multiple sections by ^L character.
;;; Author: Rui Ying
;;; Email: rui.ying@bristol.ac.uk

;;; First principal: build on demand

;;; DEPENDENCIES
;; LSP servers
;;       pylsp, clangd, fortls, texlab/digestif

;; Others:
;;       ripgrep, libvterm, multimarkdown (brew),
;;       npm package `livedown`

;; python flycheck depdencies
;; pip install pyflakes -> fast and don't check code style

;; shell checker
;; brew install shellcheck

;; (defun self/install-external-dependencies
;;     "Install external dependencies including
;;     - ripgrep
;;     - libvterm
;;     - shellchecker
;;     - py")

;; TODO
;;  - [ ] add a function to install external dependencies
;;  - [ ] configure lsp lint checker

;;; Code:

;;; FUNDEMENTAL 
;; Customize when to check package modification (much much faster)
(setq-default straight-check-for-modifications '(check-on-save find-when-checking))

;; Cause straight.el to cache the autoloads of all used packages in a single
;; file on disk thus reduce IO operations
(setq-default straight-cache-autoloads t)

;; Initialise PACKAGE MANAGER: straight.el
;; if the boostrap doesn't work, manually run
;; git clone https://github.com/raxod502/straight.el.git ~/.emacs.d/straight/repos/straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use build-in use-package (emacs > 29)
(if (version< emacs-version "29.1")
    (straight-use-package 'use-package)
  (require 'use-package))

(setq-default straight-use-package-by-default t)
(setq use-package-enable-imenu-support t)

;; Emacs Native Compilation Feature support
(when (and (fboundp 'native-comp-available-p)
	   (native-comp-available-p))
  (progn
    (setq-default native-comp-async-report-warnings-errors nil)
    (setq-default comp-deferred-compilation t)
    (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))
    (setq package-native-compile t)))

;; Update user load path
;; Optimize: Force "lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path`"
  (dolist (dir '("extra-lisp" "elpa"))
    (push (expand-file-name dir user-emacs-directory) load-path)))
(advice-add #'package-initialize :after #'update-load-path)
(update-load-path)

(when (eq system-type 'darwin)
  (defvar brew-parent-dir "/opt/homebrew/")
  (defvar brew-bin-dir (expand-file-name "bin/" brew-parent-dir))
  (defvar emacs-app-dir "/Applications/Emacs.app/")
  (defvar conda-dir "~/miniforge3/envs/workspace/"))

;; Avoid matching file name with regrex list during startup
(let ((file-name-handler-alist nil)) "~/.emacs.d/init.el")

;; Custom file
(setq-default custom-file (concat user-emacs-directory "extra-lisp/custom.el"))
(load custom-file :noerror)


;;; BUILT-IN PACKAGES

;;Coding system
;; In some old machines, you might need specify these in .bashrc
;; export LAGNUAGE=en_US.UTF-8
;; export LANG=en_US.UTF-8
;; export LC_ALL=en_US.UTF-8
;; export LC_CTYPE=en_US.UTF-8

(defvar default-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Improved global minor mode for scrolling in Emacs 29
(if (> emacs-major-version 28)
    (pixel-scroll-precision-mode)
  ;; else use third-party package
  (use-package good-scroll
    :config
    (good-scroll-mode 1)
    (global-set-key [next] #'good-scroll-up-full-screen)
    (global-set-key [prior] #'good-scroll-down-full-screen)))

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; abbreviaiont of yes/no
(if (> emacs-major-version 27)
    (setq use-short-answers t)
  (fset 'yes-or-no-p 'y-or-n-p))

;; keep .emacs.d clean
(use-package no-littering)

;;No more backup files~
(setq-default make-backup-files nil)

;;No more strange ring bell
(setq ring-bell-function 'ignore)

;; Don't ask for killing
(setq confirm-kill-processes nil)

;; Delete selection
(delete-selection-mode t)

;; right key
(context-menu-mode 1)

;; ------------------------------------------------------------
;; wrap up line (use visual-line-mode and visual-fill-column-mode instead)
;; (setq truncate-lines nil)
;; (setq-default word-wrap nil)
(use-package visual-fill-column
  :hook
  (visual-line-mode . visual-fill-column-mode)
  :custom
  (visual-fill-column-center-text t)
  ;; wrap long line
  (visual-fill-column-width 140))

;; overrides certain minor modes and variables to
;; improve the perforamce when open files with long lines
(use-package so-long
  :straight (:type built-in)
  :hook
  (after-init . global-so-long-mode)
  :custom
  (so-long-action 'so-long-minor-mode))

;; turn off non-essential reordering of bidirectional text
;; this can improve performance when dealing with large text file
(setq-default bidi-display-reordering nil)
(setq bidi-inhibit-bpa t
      long-line-threshold 1000
      large-hscroll-threshold 1000
      syntax-wholeline-max 1000)

;; auto revert buffer
(use-package autorevert
  :straight (:type built-in)
  :hook
  (after-init . global-auto-revert-mode))

;;;; Enable mouse operation in terminal emacs
(unless (display-graphic-p)
  ;; specifies the mode where <BS> or <BACKSPACE> is <DEL>
  (normal-erase-is-backspace-mode 0)
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

;; Hack clipboard for macOS in TUI mode
(defun self/kill-ring-save (orig-fun beg end &optional region)
  (unless (display-graphic-p)
    (let ((inhibit-message t))
      (shell-command-on-region beg end "pbcopy")))
  (funcall orig-fun beg end region))
(advice-add 'kill-ring-save :around #'self/kill-ring-save)

;; ---Edit keybinding style---
;; A better undo/redo mode
(use-package undo-tree
  :hook
  (after-init . global-undo-tree-mode)
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

;; undo and redo between 'buffers'
(use-package winner
  :straight (:type built-in)
  :hook
  (after-init . winner-mode)
  :bind
  (:map winner-mode-map
	("C-M-b" . winner-undo)
	("C-M-f" . winner-redo)))

;;; Auto-save buffer
(use-package real-auto-save
  :hook
  (prog-mode . real-auto-save-mode)
  (text-mode . real-auto-save-mode)
  :custom
  ;; configure time gap (in sec)
  (real-auto-save-interval 1))

;; M-w + w/l to copy/cut the word/line
(use-package easy-kill
  :bind
  ([remap kill-ring-save] . easy-kill)
  ([remap mark-sexp] . easy-mark))

;; Automatically add spacing around operators
;; use C-v to next page
(use-package electric-operator
  :hook
  (python-mode . electric-operator-mode)
  (ess-r-mode . electric-operator-mode))


;; assign every marked line a cursor
(use-package multiple-cursors
  :init
  (global-unset-key (kbd "M-<down-mouse-1>"))
  :bind
  ("C-M-<down>" . mc/mark-next-like-this)
  ("C-M-<up>" . mc/mark-previous-like-this)
  ("M-<mouse-1>" . mc/add-cursor-on-click))

(use-package ediff
  :straight (:type built-in)
  :defer t
  :config
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-highlight-all-diffs t)
  :custom
  (ediff-forward-word-function 'forward-char) ;; from https://emacs.stackexchange.com/a/9411/17066
  (ediff-highlight-all-diffs t)
  (ediff-diff-options "-w")
  (ediff-keep-variants nil)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

;;; IDE FEATURES

;; Shells in Emacs (more of an interface)
;; Tips: you can use M-r to search in shell history
;; History references like '!' (reference), ‘!!’ (last cmd) and ‘^’ (substituion, e.g., ^a^b) are supported
;; If you don't know the history reference, use C-c C-l to list all (will work for most comint buffers)
(use-package shell
  :straight (:type built-in)
  :config
  (defun no-echo-input-in-shell ()
    "Do not echo my input command"
    (setq comint-process-echoes t))
  (add-hook 'shell-mode-hook 'no-echo-input-in-shell)
  ;; can't delete output text
  (setq comint-prompt-read-only t)
  (add-hook 'comint-preoutput-filter-functions
	    (lambda (text)
	      (propertize text 'read-only t)))
  ;; kill shell buffer on exit
  (defun my-shell-exit-and-kill-buffer ()
    "Exit the shell process and kill the buffer."
    (interactive)
    (comint-send-eof)
    (let ((shell-buffer (current-buffer)))
      (run-at-time "0.1 sec" nil
		   (lambda ()
		     (when (buffer-live-p shell-buffer)
		       (kill-buffer shell-buffer))))))
  (add-hook 'shell-mode-hook
	    (lambda ()
	      (local-set-key (kbd "C-d") 'my-shell-exit-and-kill-buffer)))
  :bind
  ("C-x s" . shell)
  (:map shell-mode-map
	("C-c" . comint-interrupt-subjob)
	("<up>" . comint-previous-input)
	("C-p" . comint-previous-input)
	("<down>" . comint-next-input)
	("C-n" . comint-next-input)
	("C-l" . comint-clear-buffer)
	("SPC" . comint-magic-space)))     ;magically expand history reference

(use-package eshell-syntax-highlighting
  :hook
  ;; Enable in all Eshell buffers.
  (eshell-mode . eshell-syntax-highlighting-mode))

;; Enable this to get a superior terminal emulator (a true application like iTerm)
;; read more on https://github.com/akermu/emacs-libvterm to see the external dependencies
;; remember to check the exec-path as well
(use-package vterm
  :bind
  ("C-x t" . vterm)
  ("C-x s" . vterm-shell)
  (:map vterm-mode-map
	("C-c C-t" . vterm-copy-mode)
	("C-y" . vterm-yank))
  :custom
  (vterm-kill-buffer-on-exit t)
  (vterm-always-compile-module t)
  (vterm-max-scrollback 100000)
  :config
  (setq vterm-shell "zsh"))

;; switch between vterm and the buffer you were editing
(use-package vterm-toggle
  :straight (:host github :repo "jixiuf/vterm-toggle")
  :bind
  ("C-x t" . vterm-toggle))

;; a comint extension, e.g., :view *.jpg to view a plot in shell
;; other useful cmd: :e (edit), :ssh,
(use-package shx
  :hook
  (shell-mode . shx-global-mode))

(use-package which-key
  :hook
  (after-init . which-key-mode))

;; A dictionary inside Emacs, by abo-abo!
(use-package define-word
  :bind
  ("C-c d" . define-word-at-point)
  :config
  (setq define-word-default-service 'webster))

(use-package tempel
  :custom
  (tempel-path "~/.config/emacs/templates")
  
  ;; Require trigger prefix before template name when completing.
  :bind (("C-c i" . tempel-insert)
	 ("M-+" . tempel-complete))
  :config

  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf))

(use-package tempel-collection
  :after tempel)

;;Git + Emacs = boom!
(use-package magit
  :bind
  ("C-x g" . magit-status)
  ("C-x c" . magit-checkout)
  :config
  (setq magit-refresh-verbose t)
  (setq magit-refresh-status-buffer nil)
  (setq projectile-git-submodule-command nil)
  (setq inhibit-compacting-font-cache t)

  ;; remove hooks to speed up magit
  (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
  (remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)
  (remove-hook 'magit-revision-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)
  (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
  (remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)

  ;; remove magit-commit-diff
  (remove-hook 'server-switch-hook 'magit-commit-diff)
  (remove-hook 'with-editor-filter-visit-hook 'magit-commit-diff)

  (setq magit-show-long-lines-warning nil)
  :custom
  ;; this improve the performance of magit in MacOS
  (magit-git-executable "/usr/bin/git"))

;;a magit prefix help page
(use-package transient
  :after magit)

;; git-forge support: fetches issues, pull-requests etc.
(use-package forge
  :after magit)

;;This package reads proper environment variable in MacOS GUI version
;;To speed up this package, (1) separate configuration into
;;non-interactive (.zshenv) and interactive (.zshrc) part;
;;(2) set explicit path in .zshenv (which is what we will use, you should
;;put your PATH variable like /usr/local/bin/python3.9 in this file)
;;Find out more in https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-arguments nil) ;;read non-interactive shell config
  (exec-path-from-shell-initialize))

(use-package use-package-ensure-system-package
  :after exec-path-from-shell) ;;extend use-package, put after exec-path-from-shell

;; pop up window management
(use-package popwin
  :hook
  (after-init . popwin-mode))

;; better isearch
(use-package ctrlf
  :hook
  (after-init . ctrlf-mode)
  :config
  (setq ctrlf-default-search-style 'fuzzy)
  :bind
  ("C-s" . ctrlf-forward-fuzzy-regexp)
  ("C-r" . ctrlf-backward-fuzzy-regexp))
  
;; completion UI
(use-package vertico
  :hook
  (after-init . vertico-mode))

;; use posframe (in the centre of buffer) for vertico
(use-package vertico-posframe
  :if window-system
  :hook
  (vertico-mode . vertico-posframe-mode))

;; Emacs 28: Hide commands in M-x which do not work in the current mode.
;; Vertico commands are hidden in normal buffers.
(setq read-extended-command-predicate
      #'command-completion-default-include-p)

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)

;; save minibuffer history
(use-package savehist
  :config
  (savehist-mode))

;; save the cursor location
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(use-package crux
  :bind (("C-a" . crux-move-beginning-of-line)
         ("C-x 4 t" . crux-transpose-windows)
         ("C-x K" . crux-kill-other-buffers)
         ("C-k" . crux-smart-kill-line)
         ("C-c r" . crux-rename-file-and-buffer)
         ("C-x DEL" . crux-kill-line-backwards))
  :config
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify)
  (crux-with-region-or-point-to-eol kill-ring-save)
  (defalias 'rename-file-and-buffer #'crux-rename-file-and-buffer))

;; completion strategy
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; rich annotations of minibuffer
(use-package marginalia
  :hook
  (after-init . marginalia-mode))

(use-package emojify
  :config
  (when (member "Segoe UI Emoji" (font-family-list))
    (set-fontset-font
     t 'symbol (font-spec :family "Segoe UI Emoji") nil 'prepend))
  (setq emojify-display-style 'unicode)
  (setq emojify-emoji-styles '(unicode))
  :bind
  ("C-c ." . emojify-insert-emoji))

(use-package corfu
  :hook
  (after-init . corfu-mode))

(use-package corfu-terminal
  :if (not window-system)
  :hook
  (after-init . corfu-terminal-mode))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; language spell checker
(use-package jit-spell
  :hook
  (text-mode . jit-spell-mode)
  :bind
  (:map jit-spell-mode-map
	("C-;" . jit-spell-correct-word)))

;; completion-at-point extensions for corfu
(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
	 ("C-c p t" . complete-tag)	   ;; etags
	 ("C-c p d" . cape-dabbrev)	   ;; or dabbrev-completion
	 ("C-c p h" . cape-history)
	 ("C-c p f" . cape-file)
	 ("C-c p k" . cape-keyword)
	 ("C-c p s" . cape-symbol)
	 ("C-c p a" . cape-abbrev)
	 ("C-c p l" . cape-line)
	 ("C-c p \\" . cape-tex)
	 ("C-c p _" . cape-tex)
	 ("C-c p ^" . cape-tex)
	 ("C-c p &" . cape-sgml)
	 ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-sgml)
  (add-to-list 'completion-at-point-functions #'cape-rfc1345)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)

  (add-to-list 'completion-at-point-functions #'cape-symbol)
  (add-to-list 'completion-at-point-functions #'cape-line))

;; a bunch of advanced commands: buffer switching, imenu, search commands etc.
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
	 ("<f3>" . consult-recent-file)
	 ("C-c h" . consult-history)
	 ("C-c m" . consult-mode-command)

	 ;; C-x bindings (ctl-x-map)
	 ("C-x b" . consult-buffer)
	 ;; orig. switch-to-buffer
	 ("C-x r b" . consult-bookmark) ;; orig. bookmark-jump

	 ;; Other custom bindings
	 ("M-y" . consult-yank-pop) ;; orig. yank-pop
	 
	 :map isearch-mode-map
	 ("M-e" . consult-isearch-history) ;; orig. isearch-edit-string
	 ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
	 ("M-s L" . consult-line-multi) ;; needed by consult-line to detect isearch
	 ;; Minibuffer history
	 :map minibuffer-local-map
	 ("M-s" . consult-history) ;; orig. next-matching-history-element
	 ("M-r" . consult-history)) ;; orig. previous-matching-history-element

  :hook
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  (completion-list-mode . consult-preview-at-point-mode)

  :config
  (advice-add #'project-find-regexp :override #'consult-ripgrep)
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref))

;; consult extensions
(use-package consult-flycheck
  :after (consult flycheck))

;; project buffer/file
(use-package consult-projectile
  :after (consult projectile))

;; Built-in project manager, support git repos only
(use-package projectile
  :hook
  (prog-mode . projectile-mode)
  :config
  ;; to avoid slowness over tramp 
  (defadvice projectile-project-root (around ignore-remote first activate)
    (unless (file-remote-p default-directory) ad-do-it))
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map))

;; robust find file (at point) in project
(use-package find-file-in-project
  :bind
  ("C-x f" . find-file-in-project-at-point))

(use-package recentf
  :hook
  (after-init . recentf-mode))

;;Faster cursor movement - go to anywhere
(use-package avy
  :bind 
  ("C-'" . avy-goto-char-2)		;; input: two characters
  ("M-g l" . avy-goto-line))

(use-package ace-window
  :bind
  ("M-o" . ace-window)
  ("C-x o" . ace-swap-window))

;; restart emacs
(use-package restart-emacs
  :commands (restart-emacs))

(use-package esup
  :config
  (setq esup-depth 0))



;;; KEYBINDING

;; Set meta command for Mac OS
;; If you are using a external Windows keyboard, remeber to choose
;; USB keyboard in Preference -> Keyboard -> modify keyboard -> select keyboard
(if (eq system-type 'darwin)
    (setq mac-command-modifier 'meta))

(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-M-;") 'comment-box)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-x ,") 'beginning-of-buffer)
(global-set-key (kbd "C-x .") 'end-of-buffer)

;; globally go to previous position; "C-u C-SPC" to do same locally
(global-set-key (kbd "C-c C-SPC") 'pop-global-mark)
;; repeat command
(global-set-key (kbd "<f4>") #'repeat)

;; M-up/down to move text
(use-package move-text
  :config
  (move-text-default-bindings))

(defun select-current-line ()
  "Select the current line."
  (interactive)
  (move-beginning-of-line 1)
  (push-mark nil nil t)
  ;;(forward-line 1)
  (end-of-line 1))

(global-set-key (kbd "C-l") 'select-current-line)

(defvar self/init-file-path (expand-file-name (concat user-emacs-directory "init.el")))
(defun self/open-init-file()
  "Open my init.el."
  (interactive)
  (find-file self/init-file-path))
(global-set-key (kbd "<f2>") 'self/open-init-file)

;; deleting a whitespace character will delete all whitespace
(use-package hungry-delete
  :hook
  (after-init . global-hungry-delete-mode)
  :config
  ;; left one last whitespace
  (setq hungry-delete-join-reluctantly t))

;; dynamic module required
(use-package rime
  :config
  (setq rime-show-candidate 'posframe)
  (setq rime-posframe-properties
	(list :internal-border-width 1))
  (setq rime-inline-ascii-trigger 'shift-l)
  :custom
  (default-input-method "rime")
  ;; the path of emacs_module.h
  (rime-emacs-module-header-root (concat emacs-app-dir "Contents/Resources/include"))
  ;; configuration path
  (rime-librime-root (concat user-emacs-directory "librime/dist"))
  (rime-user-data-dir (concat user-emacs-directory "rime")))

;; Mark set
;; C-x h to select all
;; C-u C-SPC to go back (mark ring)

(use-package general
  :config
  ;; leader key
  (defconst leader "\\")
  (general-create-definer my/leader-def
    :prefix leader)

  ;; double press to input `\`
  (defun quote-backslash ()
    (interactive)
    (insert "\\"))

  ;; ------ Global Keybindings ------
  (my/leader-def
    "" nil
    "\\" 'quote-backslash

    ;; move cursor
    "m l" '(avy-goto-line :which-key "goto-line")
    "m g" '(goto-line :which-key "goto-line-number")
    "m m" '(exchange-point-and-mark :which-key "go-back-and-mark")
    "m b" '(consult-global-mark :which-key "go-back")

    ;; change  indent
    "<tab>" '(indent-rigidly :which-key "move code")
    
    ;; project operations 
    "p p" '(projectile-switch-project :which-key "project switch")
    "p s" '(consult-ripgrep :which-key "project search text")
    "p b" '(consult-projectile :which-key "project buffer/file")
    "p f" '(projectile-find-file :which-key "project find file")
    "p F" '(fzf-find-file-in-dir :which-key "project fuzzy find file")
    "p i" '(consult-imenu :which-key "project imenu")
    
    ;; shell/terminal
    "p v" '(projectile-run-vterm :which-key "project vterm")
    "p x" '(projectile-run-shell :which-key "project shell")
    "p e" '(projectile-run-eshell :which-key "project eshell")
    "p c" '(projectile-compile-project :which-key "project compile")

    ;; bookmark
    "b m" '(bookmark-set :which-key "bookmark set")
    "b l" '(bookmark-bmenu-list :which-key "bookmark list")
    "b g" '(bookmark-jump :which-key "bookmark GO!")

    ;; org mode
    "o c" '(org-capture :which-key "org capture")

    "e b" '(ediff-buffers :which-key "compare buffers")
    "e f" '(ediff-files :which-key "compare files")

    "h a" '(mark-whole-buffer :which-key "select all")

    "." 'mc/mark-next-like-this
    "," 'mc/mark-previous-like-this

    "1" 'beginning-of-buffer
    "2" 'end-of-buffer

    "v" 'vterm
    "s" 'shell
    "g" '(magit-status :which-key "git")
    "f" 'find-file
    "k" 'kill-this-buffer
    "r" 'restart-emacs
    "q" 'save-buffers-kill-terminal)

  ;; ------ Mode-specific Keybindings ------
  (my/leader-def prog-mode-map
    "i" 'consult-imenu
    "s" 'shell
    "v" 'vterm
    "c" 'lsp-bridge-diagnostic-list
    "%" 'query-replace)

  (my/leader-def text-mode-map
    "d" 'define-word-at-point)

  (my/leader-def markdown-mode-map
    "l" 'livedown-preview
    "k" 'livedown-kill)

  (my/leader-def python-mode-map
    "r" 'python-shell-send-buffer
    "h" 'eldoc)

  (my/leader-def ess-r-mode-map
    "r" 'ess-eval-buffer-and-go
    "h" 'eldoc)

  (my/leader-def dired-mode-map
    "e" 'dired-toggle-read-only
    "r" 'dired-rsync
    ))

;; adjust font size
(setq-default text-scale-mode-step 1.1)

(use-package helpful
  :bind
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  ("C-h d" . helpful-at-point)
  ("C-h C" . helpful-command)
  ("C-h F" . helpful-function))

;; select one and edit all (https://github.com/victorhge/iedit)
;; iedit is also dependency of lispy, use M-i to toggle
(use-package iedit
  :bind
  ("M-i" . iedit-mode))

;; displays current match and total matches in search
;; e.g., anzu-query-replace: same but displace selection
(use-package anzu
  :hook
  (prog-mode . global-anzu-mode)
  :bind
  ("C-c %" . anzu-query-replace)
  ("C-c r" . anzu-query-replace-regexp))

(use-package highlight-symbol
  ;; highlight the other symbols matching current cursor   
  :bind
  ("C-<f9>" . highlight-symbol)
  ("<f9>" . highlight-symbol-next)
  ("S-<f9>" . highlight-symbol-prev)
  ("M-<f9>" . highlight-symbol-query-replace))

;;; WINDOW, UI & APPEARANCE

;;If you are running Emacs in MacOS, then I recommend you using
;;Emacs-mac <--with-no-title-bars> which improves GUI performance a lot
;;Also: for the Emacs-mac you can swipe between buffer by using two fingers (cool!)
(when (eq system-type 'darwin)
  (progn
    (setq dired-use-ls-dired nil) ;;to avoid error "ls does not support --dired" in MacOS
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
    (add-to-list 'default-frame-alist '(ns-appearance . light))))

(defun auto-max-frame ()
  "Maxize/full screen the frame according to the OS type."
  (interactive)
  (if (eq system-type 'darwin)
      (toggle-frame-maximized)
    (toggle-frame-fullscreen)))
(auto-max-frame)

;;Cursor
(setq-default cursor-type 'bar)
(blink-cursor-mode 0)

;;Highlight current line, based on default hl-line-mode
(use-package lin
  :hook
  (prog-mode . lin-mode)
  (text-mode . lin-mode))

;; highlight cursor when scroll window
(use-package beacon
  :straight (:host github :repo "Malabarba/beacon")
  :hook
  (after-init . beacon-mode))

;; minimal columns for Emacs to split window horizontally
(setq split-width-threshold 130)

;; Change cursor color dynamically at cursor or pointer
(use-package smart-cursor-color
  :hook
  (after-init . smart-cursor-color-mode))

(setq x-underline-at-descent-line t)

;;Display line number
(global-display-line-numbers-mode t) ;;the linum-mode has been obsolete
(setq display-line-numbers-width 0)

;; Line annotation for changed and saved lines.
(use-package line-reminder
  :hook
  (after-init . global-line-reminder-mode)
  :config
  (setq line-reminder-show-option 'indicators))

;;Disable line number for certain modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; code formatting, require third-party formatter
(use-package format-all
  :hook
  ;; turn on format-all-mode which
  ;; automatically format code on save
  (prog-mode . format-all-mode))

;; stop automatically indents the line
(electric-indent-mode nil)

(setq frame-inhibit-implied-resize nil)

;;-----------Dired setting/replacement-------------
(use-package dired
  :straight (:type built-in)
  :config
  (setq dired-listing-switches "-alFhv")
  (setq dired-dwim-target t)
  (setq dired-dwim-target t)
  :bind
  ;; % - m to mark regex
  (:map dired-mode-map
	("o" . dired-display-file)
	("<mouse-2>" . dired-mouse-find-file)))

(defun hide-dired-mode-info ()
  "show less information in dired buffers"
  (dired-hide-details-mode 1))
(add-hook 'dired-mode-hook 'hide-dired-mode-info)

(use-package dired-hacks-utils
  :hook
  (dired-mode . dired-utils-format-information-line-mode)
  :bind
  (:map dired-mode-map
	("j" . dired-hacks-next-file)
	("k" . dired-hacks-previous-file)))

;; to replace slow tramp copy
;; maybe need to reinstall rsync in MacOS
(use-package dired-rsync
  :bind
  (:map dired-mode-map
	("C-c C-r" . dired-rsync)))

(use-package pulsing-cursor
  :straight (:type git :host github
		   :repo "jasonjckn/pulsing-cursor")
  :hook
  (prog-mode . pulsing-cursor-mode))

;;--------------------------------------------------
;; Matching parenthesis
;;--------------------------------------------------

;; Showing matching parentheses (built-in)
(use-package highlight-parentheses
  :hook
  (after-init . global-highlight-parentheses-mode)
  :config
  (setq highlight-parentheses-highlight-adjacent t))

;;--> Option 1 (built-in)
(electric-pair-mode t)
(setq electric-pair-pairs '((?\" . ?\")
			    (?\` . ?\`)
			    (?\( . ?\))
			    (?\{ . ?\})))

;; use lispy-mode (a vi-like editing) for lisp parentheses
;; remove electric-pair-mode first
(add-hook 'emacs-lisp-mode-hook (lambda ()
				  (electric-pair-local-mode -1)))

;; >>> Basic lispy usage:
;; jkhl to move, f/b to foward/backward level
;; c to copy, m to mark, e to evaluate, d to switch parenthesis side (C-d to delete)
;; >/< to slurp/barf: push out/pull in
;; w/s to move marked regions up/down
;; M-j to split, + to join
;; To insert a single parenthsis, use a C-q prefix
;; M-x check-parens to check unbalanced parens

(use-package lispy
  :hook
  (emacs-lisp-mode . lispy-mode)
  :bind
  ("C-M-l" . lispy-mode)
  (:map lispy-mode-map
	("M-o" . ace-window))
  :config
  ;; allow delete single parenthesis
  (define-key lispy-mode-map-lispy (kbd "C-d") nil))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package minions
  :config
  (minions-mode 1)
  :bind
  ([S-down-mouse-3] . minions-minor-modes-menu))

(use-package highlight-indent-guides
  :hook
  (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character))

;; (use-package indent-bars
;;   :straight (indent-bars :type git :host github :repo "jdtsmith/indent-bars")
;;   :custom
;;   (indent-bars-treesit-support t)
;;   (indent-bars-no-descend-string t)
;;   (indent-bars-treesit-ignore-blank-lines-types '("module"))
;;   :hook ((prog-mode) . indent-bars-mode)
;;   :config
;;   (setq
;;    indent-bars-color '(highlight :face-bg t :blend 0.3)
;;    indent-bars-pattern " . . . . ." ; play with the number of dots for your usual font size
;;    indent-bars-width-frac 0.25
;;    indent-bars-pad-frac 0.1)
;;   )

(use-package all-the-icons
  :if (display-graphic-p)
  :config
  (setq inhibit-compacting-font-caches t))

(use-package all-the-icons-dired
  :if window-system
  ;;need to run all-the-icons-install-fonts first to avoid grabled icon
  :hook
  (dired-mode . all-the-icons-dired-mode))

;; all the icons for completion framework (e.g. vertico)
(use-package all-the-icons-completion
  :hook
  (after-init . all-the-icons-completion-mode)
  :config
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))

;; (global-tab-line-mode t)

;; to display ^L page break
(use-package form-feed
  :hook
  (emacs-lisp-mode . form-feed-mode))


;;; FONT, THEME & COLOR SCHEME

;;English font: Iosevka/Inconsolata/Juliamono/Jetbrains Mono/Roboto Mono/Monaco/Fira Code/SF Mono/IBM Plex Mono/Anonymous Pro
;;Chinese font: Wenquanyi Micro Hei Mono/Sarasa UI SC Mono/Sarasa Mono SC Nerd/Noto Sans CJK SC Mono/LXGW WenKai (work perfectly with Iosevka/Inconsolata)
;;Variable-pitch font, ETBembo/New York
;;Unicode: Symbola

(defun self/setup-font ()
  "Set English and CJK font for Emacs."
  (interactive)
  ;; English font
  (if (display-graphic-p)
      (progn
	;; English font
	(set-face-attribute 'default nil :font (format "%s:pixelsize=%d" "Monaco" 13.5))
	;; CJK font
	(dolist (charset '(kana han symbol cjk-misc bopomofo))
	  (set-fontset-font (frame-parameter nil 'font)
			    charset
			    (font-spec :family "LXWG WenKai"))))))

;; Use emacs daemon, put following lines to shell config file
;; alias emacs=/path_to_miniconda3/bin/emacs
;; alias emacsclient=/path_to_miniconda/bin/emacsclient
;; alias ed="emacs --daemon"
;; alias ec="emacsclient -c"
;; alias eq="emacsclient -e '(save-buffers-kill-emacs)'"
;; If you want more fuzzy cmd:
;; alias emcas=emacs
;; alias emasc=emacs
;; alias enacs=emacs

;; Set font and auto-fullscreen in daemon-mode, put after init-ui.el
(if (daemonp)
    (add-hook 'after-make-frame-functions
	      (lambda (frame)
		(with-selected-frame frame
		  (self/setup-font)
		  (auto-max-frame))))
  (add-hook 'after-init-hook 'self/setup-font))

;; lazy-load default theme
(setq custom-safe-themes t)

(use-package ef-themes
  :config
  (load-theme 'ef-light))

;; mode line
(use-package mood-line
  :hook
  (after-init . mood-line-mode))

;; colorful compilation buffer
(use-package fancy-compilation
  :commands (fancy-compilation-mode)
  :hook
  (fancy-compilation-mode . compilation-mode))

;; dim inactive buffer
(use-package auto-dim-other-buffers
  :hook
  (after-init . auto-dim-other-buffers-mode)
  :custom
  (auto-dim-other-buffers-face "white smoke"))

;;Transprancy setting
(set-frame-parameter (selected-frame) 'alpha '(97 100))
(add-to-list 'default-frame-alist '(alpha 97 100))

;;Font Setting
(setq inhibit-compacting-font-caches t)

;;Varialble/fixed pictch font setting, essential for org-mode
(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "LXWG WenKai " :height 160))))
 '(fixed-pitch ((t ( :family " Iosevka" :height 160)))))


;;==============================
;;; PROGRAMMING LANGUAGES & LSP
;;==============================

;; Install treesit C-library first: `brew install tree-sitter'
;; Automatically install and use tree-sitter major modes in Emacs 29+
(use-package treesit-auto
  :config
  (global-treesit-auto-mode))

(use-package yasnippet
  :after elpy)

;; Language Server Protocol Implementation
;; require `epc` python libary
(use-package lsp-bridge
  :if (window-system)
  :straight (lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
			:files ("*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources"))
  :hook
  (prog-mode . lsp-bridge-mode)
  (latex-mode . lsp-bridge-mode)
  (LaTeX-mode . lsp-bridge-mode)
  :config
  (setq lsp-bridge-python-command python-shell-interpreter)
  (setq lsp-bridge-user-langserver-dir (expand-file-name "langserver" user-emacs-directory))
  :bind
  (:map lsp-bridge-mode-map
	("C-c r" . lsp-bridge-rename)
	("M-." . lsp-bridge-find-def)
	("<return>" . acm-complete))
  :custom
  (acm-enable-yas nil)
  (acm-enable-tabnine nil)
  (acm-enable-copilot t)
  (lsp-bridge-c-lsp-server "clangd")
  (lsp-bridge-python-lsp-server "pyright")
  (lsp-bridge-tex-lsp-server "texlab"))

;; AI assisted completion
(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :hook
  (prog-mode . copilot-mode)
  :config
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))

;; show tree-like structure of current position
(use-package breadcrumb  
  :straight (breadcrumb :type git
			:host github
			:repo "joaotavora/breadcrumb")
  :hook
  (prog-mode . breadcrumb-mode))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

;;==============================
;;           Python           ;;
;;==============================

;; Interpreter choice, use `run-python' to find current interpreter`
(setq python-shell-interpreter (expand-file-name "bin/python" conda-dir))

;;python-style indent
(setq python-indent-offset 4)
(setq python-indent-guess-indent-offset-verbose nil) ;;don't complain about the indent anymore
(setq python-indent-guess-indent nil)
(setq indent-tabs-mode t) ;; whether tabs are used for indentation

;;python-mode local keybinding
(with-eval-after-load 'python
  (defun python-run-current-line ()
    "a wrapper of python-shell-send-statement"
    (interactive)
    (python-shell-send-statement)
    (forward-line))
  (define-key python-mode-map (kbd "C-<return>") 'python-run-current-line)
  (define-key inferior-python-mode-map (kbd "C-l") 'comint-clear-buffer)
  ;; (define-key inferior-python-mode-map (kbd "<up>") 'comint-previous-input)
  (define-key inferior-python-mode-map (kbd "C-p") 'comint-previous-input)
  ;; (define-key inferior-python-mode-map (kbd "<down>") 'comint-next-input)
  (define-key inferior-python-mode-map (kbd "C-n") 'comint-next-input))

(use-package elpy
  :after python
  :requires yasnippet
  :config
  (elpy-enable))

;;==============================
;;           Rlang            ;;
;;==============================
;; require ESS installed
;;Lazy load ess-r-mode (ESS doesn't like use-package pretty much)
(use-package ess
  :defer t)

(add-to-list 'auto-mode-alist '("\\.R\\'" . ess-r-mode))
(with-eval-after-load 'ess-r-mode
  (defun ess-insert-pipe ()
    "Insert a R pipe (%>%)"
    (interactive)
    (just-one-space 1)
    (insert "%>%")
    (just-one-space 1)
    ;;(reindent-then-newline-and-indent)
    )

  (defun ess-clear-REPL-buffer ()
    "Clear outputs in the REPL buffer"
    (interactive)
    (let ((r-repl-buffer (seq-find (lambda (buf)
				     (string-prefix-p "*R" (buffer-name buf)))
				   (buffer-list))))
      (if r-repl-buffer
	  (with-current-buffer r-repl-buffer
	    (comint-clear-buffer))
	(user-error "No R REPL buffers found"))))

  (define-key ess-r-mode-map (kbd "C-l") 'ess-clear-REPL-buffer)
  (define-key inferior-ess-r-mode-map (kbd "C-l") 'ess-clear-REPL-buffer) ;;inferior-* is the shell one
  (define-key ess-r-mode-map (kbd "M--") 'ess-insert-assign)
  (define-key inferior-ess-r-mode-map (kbd "M--") 'ess-insert-assign)
  (define-key ess-r-mode-map (kbd "M-p") 'ess-insert-pipe)
  (define-key inferior-ess-r-mode-map (kbd "M-p") 'ess-insert-pipe)
  (define-key inferior-ess-r-mode-map (kbd "C-p") 'comint-previous-input)
  ;; (define-key inferior-ess-r-mode-map (kbd "<up>") 'comint-previous-input)
  (define-key inferior-ess-r-mode-map (kbd "C-n") 'comint-next-input)
  ;; (define-key inferior-ess-r-mode-map (kbd "<down>") 'comint-next-input)
  )

;;C-c C-a to turn on csv-align-fields
(use-package csv-mode
  :mode
  "\\.csv\\'"
  "\\.CSV\\'")

;;display color of RGB code
(use-package rainbow-mode
  :hook
  (ess-r-mode . rainbow-mode)
  (prog-mode . rainbow-mode))

;;;;;;;;;;
;; Yaml ;;
;;;;;;;;;;

(use-package yaml-mode
  :mode "\\.yml\\'"
  :bind
  (:map yaml-mode-map
	("\C-m" . newline-and-indent)))

(use-package cmake-mode
  :mode ("\\.cmake\\'" "CMakeLists\\.txt\\'"))

;;;;;;;;;;;;;;
;; LaTeX    ;;
;;;;;;;;;;;;;;

;; AUCTEX
;; all in one: C-c C-c
(use-package auctex
  :ensure t
  :no-require t
  :config
  ;; get support for many of the LaTeX packages in your documents
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)

  ;; pdf viewer: Skim
  (setq TeX-view-program-list
	'(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))

  (setq TeX-view-program-selection '((output-pdf "Skim")))
  ;; Forward search happens automatically upon calling the viewer,
  (setq TeX-source-correlate-method 'synctex)
  :hook
  ;; SyncTeX setup
  (LaTeX-mode . TeX-source-correlate-mode)
  :bind
  (:map LaTeX-mode-map
	("C-c C-a" . TeX-command-run-all)
	("M-<mouse-1>" . TeX-view)))

;; Enable RefTeX for managing cross-references, citations, and labels
;; (use-package reftex
;;   :commands (turn-on-reftex)
;;   :init
;;   (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;;   (setq reftex-plug-into-AUCTeX t))

;; for fast insertion symbols
;; C-c $, or ` to insert math symbol
;; (use-package cdlatex
;;   :hook
;;   (LaTeX-mode . turn-on-cdlatex))

;; bookmark is for quickly jumping to a file/location
(use-package bookmark
  :defer t
  :straight (:type built-in)
  :config
  (bookmark-bmenu-list))


;;; Text-mode: Markdown/org-mode/TeX

;;;;;;;;;;;;;;
;; Markdown ;;
;;;;;;;;;;;;;;

;; Major mode for markdown
;; preview included but reply on multimarkdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :hook
  (markdown-mode . (lambda ()
		     (display-line-numbers-mode -1)
		     (visual-line-mode 1))))

;; Advanced preview
;; Dependency: npm/node, livedown npm package
(use-package emacs-livedown
  :after markdown-mode
  :straight (:type git
		   :host github
		   :repo "shime/emacs-livedown")
  :custom
  (livedown-autostart t) ; automatically open preview when opening markdown files
  (livedown-open t)	 ; automatically open the browser window
  (livedown-port 1337)	 ; port for livedown server
  (livedown-browser nil) ; browser to use
  :bind
  (:map markdown-mode-map
	("C-c c p" . livedown-preview)
	("C-c c k" . livedown-kill)))

;;add table of content for md/org
;;Add :TOC: tag for org (C-c C-c) and <-- :TOC: --> for md
;;then toc-org-insert-toc
(use-package toc-org
  :hook
  (markdown-mode . toc-org-mode)
  (org-mode . toc-org-mode)
  :config
  (global-set-key (kbd "\C-c\C-o") 'toc-org-markdown-follow-thing-at-point)
  (add-to-list 'org-tag-alist '("TOC" . ?T)))

;;===========
;; Org-mode
;;===========

(use-package org
  :straight (:type built-in)
  :custom
  ;; set default note file
  (org-directory "~/Documents")
  (org-default-notes-file (concat org-directory "/TODO.org"))

  :custom-face
  ;; source code block line
  (org-block-begin-line ((t (:underline t :background unspecified))))
  (org-block-end-line ((t (:overline t :underline nil :background unspecified))))

  :config
  (setq org-hide-block-startup t)
  ;; learn from: https://github.com/Elilif/.elemacs
  (defun eli-hide-org-block-begin-line (orig from to flag spec)
    (if (eq spec 'org-hide-block)
	(let* ((beg-of-line (save-excursion
			      (beginning-of-line)
			      (point)))
	       (lang (car (org-babel-get-src-block-info)))
	       (beg (+ beg-of-line 12 (length lang))))
	  (funcall orig beg to flag spec))
      (funcall orig from to flag spec)))

  (advice-add 'org-flag-region :around #'eli-hide-org-block-begin-line)

  (setq org-todo-keywords
	'((sequence "TODO(t)" "DOING(s)" "|" "DONE(d!/!)")))

  ;; capture TODOs: %U -> dat; %i -> current selection;
  (setq org-capture-templates
	'(("t" "Todo" entry (file+headline "~/Documents/TODO.org" "Tasks")
	   "* TODO [#A] %? %i %U"
	   :empty-lines 1)))

  (setq org-fontify-quote-and-verse-blocks t)


  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (shell . t)))

  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :weight bold))))
   '(org-level-2 ((t (:inherit outline-2 :weight normal))))
   '(org-level-3 ((t (:inherit outline-3 :weight normal))))
   '(org-level-4 ((t (:inherit outline-4 :weight normal))))
   '(org-level-5 ((t (:inherit outline-5 :weight normal))))
   '(org-level-6 ((t (:inherit outline-6 :weight normal))))
   '(org-level-7 ((t (:inherit outline-7 :weight normal))))
   '(org-level-8 ((t (:inherit outline-8 :weight normal)))))

  ;; emphasize
  (defface org-bold
    '((t :foreground "#d2268b"
	 :background "#fefefe"
	 :weight bold
	 :underline t
	 :overline t))
    "Face for org-mode bold."
    :group 'org-faces)

  (setq org-emphasis-alist
	'(("*" org-bold)
          ("/" italic)
          ("_" underline)
          ("=" (:background "maroon" :foreground "white") org-verbatim)
          ("~" (:background "deep sky blue" :foreground "MidnightBlue") org-code)
          ("+" (:strike-through t) org-code)))

  (set-face-background 'org-bold "#fefefe")
  (set-face-background 'org-verbatim "#fefefe")

  :bind
  (:map org-mode-map
	("C-c s" . org-insert-structure-template))

  :hook
  ;; pretty symbol for org-mode
  (org-mode . (lambda ()
		;; tickboxes
		(push '("[ ]" . "🞎") prettify-symbols-alist)
		(push '("[X]" . "☑") prettify-symbols-alist)
		(push '("[-]" . "◫") prettify-symbols-alist)
		(push '("!=" . "≠") prettify-symbols-alist)
		;; arrows
		(push '("->" . "→") prettify-symbols-alist)
		(push '("<-" . "←") prettify-symbols-alist)
		(push '("=>" . "⇒") prettify-symbols-alist)
		(push '("<=" . "⇐") prettify-symbols-alist)
		(push '("\\->" . "↳") prettify-symbols-alist)
		(push '("<-/" . "↵") prettify-symbols-alist)
		(prettify-symbols-mode))))

;; Beautify org-mode
(use-package org-superstar
  :hook
  (org-mode . org-superstar-mode))

(use-package svg-tag-mode
  :hook (org-mode . svg-tag-mode)
  :config
  (defun mk/svg-checkbox-empty ()
    (let* ((svg (svg-create 14 14)))
      (svg-rectangle svg 0 0 14 14 :fill 'white :rx 2 :stroke-width 2.5 :stroke-color 'black)
      (svg-image svg :ascent 'center)))

  (defun mk/svg-checkbox-filled ()
    (let* ((svg (svg-create 14 14)))
      (svg-rectangle svg 0 0 14 14 :fill "#FFFFFF" :rx 2)
      (svg-polygon svg '((5.5 . 11) (12 . 3.5) (11 . 2) (5.5 . 9) (1.5 . 5) (1 . 6.5))
		   :stroke-color 'black :stroke-width 1 :fill 'black)
      (svg-image svg :ascent 'center)))

  (defun mk/svg-checkbox-toggle ()
    (interactive)
    (save-excursion
      (let* ((start-pos (line-beginning-position))
	     (end-pos (line-end-position))
	     (text (buffer-substring-no-properties start-pos end-pos))
	     (case-fold-search t)  ; Let X and x be the same in search
	     )
	(beginning-of-line)
	(cond ((string-match-p "\\[X\\]" text)
	       (progn
		 (re-search-forward "\\[X\\]" end-pos)
		 (replace-match "[ ]")))
	      ((string-match-p "\\[ \\]" text)
	       (progn
		 (search-forward "[ ]" end-pos)
		 (replace-match "[X]")))))))

  (defun svg-progress-percent (value)
    (svg-image (svg-lib-concat
		(svg-lib-progress-bar (/ (string-to-number value) 100.0)
				      nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
		(svg-lib-tag (concat value "%")
			     nil :stroke 0 :margin 0)) :ascent 'center))

  (defun svg-progress-count (value)
    (let* ((seq (mapcar #'string-to-number (split-string value "/")))
	   (count (float (car seq)))
	   (total (float (cadr seq))))
      (svg-image (svg-lib-concat
		  (svg-lib-progress-bar (/ count total) nil
					:margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
		  (svg-lib-tag value nil
			       :stroke 0 :margin 0)) :ascent 'center)))

  (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
  (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
  (defconst day-re "[A-Za-z]\\{3\\}")
  (defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))

  (setq svg-tag-action-at-point 'edit)

  (setq svg-lib-icon-collections
	`(("bootstrap" .
	   "https://icons.getbootstrap.com/assets/icons/%s.svg")
	  ("simple" .
	   "https://raw.githubusercontent.com/simple-icons/simple-icons/develop/icons/%s.svg")
	  ("material" .
	   "https://raw.githubusercontent.com/Templarian/MaterialDesign/master/svg/%s.svg")
	  ("octicons" .
	   "https://raw.githubusercontent.com/primer/octicons/master/icons/%s-24.svg")
	  ("boxicons" .
	   "https://boxicons.com/static/img/svg/regular/bx-%s.svg")))

  (setq svg-tag-tags
	`(
	  ;; Task priority
	  ("\\[#[A-Z]\\]" . ((lambda (tag)
			       (svg-tag-make tag :face 'org-priority
					     :beg 2 :end -1 :margin 0))))

	  ;; Progress
	  ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
					      (svg-progress-percent (substring tag 1 -2)))))
	  ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
					    (svg-progress-count (substring tag 1 -1)))))

	  ;; Checkbox
	  ("\\[ \\]" . ((lambda (_tag) (mk/svg-checkbox-empty))
			(lambda () (interactive) (mk/svg-checkbox-toggle))
			"Click to toggle."))
	  ("\\(\\[[Xx]\\]\\)" . ((lambda (_tag) (mk/svg-checkbox-filled))
				 (lambda () (interactive) (mk/svg-checkbox-toggle))
				 "Click to toggle."))

	  ;; Active date (with or without day name, with or without time)
	  (,(format "\\(<%s>\\)" date-re) .
	   ((lambda (tag)
	      (svg-tag-make tag :beg 1 :end -1 :margin 0))))
	  (,(format "\\(<%s \\)%s>" date-re day-time-re) .
	   ((lambda (tag)
	      (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
	  (,(format "<%s \\(%s>\\)" date-re day-time-re) .
	   ((lambda (tag)
	      (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))

	  ;; Inactive date  (with or without day name, with or without time)
	  (,(format "\\(\\[%s\\]\\)" date-re) .
	   ((lambda (tag)
	      (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
	  (,(format "\\(\\[%s \\)%s\\]" date-re day-time-re) .
	   ((lambda (tag)
	      (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))
	  (,(format "\\[%s \\(%s\\]\\)" date-re day-time-re) .
	   ((lambda (tag)
	      (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date))))

	  ;; Keywords
	  ("TODO" . ((lambda (tag) (svg-tag-make tag :height 0.8 :inverse t
						 :face 'org-todo :margin 0 :radius 5))))
	  ("WORK" . ((lambda (tag) (svg-tag-make tag :height 0.8
						 :face 'org-todo :margin 0 :radius 5))))
	  ("DONE" . ((lambda (tag) (svg-tag-make tag :height 0.8 :inverse t
						 :face 'org-done :margin 0 :radius 5))))

	  ("FIXME\\b" . ((lambda (tag) (svg-tag-make "FIXME" :face 'org-todo :inverse t :margin 0 :crop-right t))))

	  ;; beautify pagebreak in orgmode
	  ("\\\\pagebreak" . ((lambda (tag) (svg-lib-icon "file-break" nil :collection "bootstrap"
							  :stroke 0 :scale 1 :padding 0)))))))
    

;; Perfectly alian English/CJK fonts in the same table
(use-package valign
  :hook (org-mode . valign-mode)
  :config
  (setq valign-fancy-bar t))


;;; Final cook up

;; Back to normal GC level
(defun self/cleanup-gc ()
  "Clean up gc.  From user redguardtoo."
  (setq gc-cons-threshold 100000000)
  (setq gc-cons-percentage 0.1))

;; Collect gc during free time
(run-with-idle-timer 4 nil #'self/cleanup-gc)

(setq max-specpdl-size 32000
      max-lisp-eval-depth 16000)

(defun self/print-init-info ()
  "Print init time of Emacs, a wrapper of 'emacs-init-time."
  (interactive)
  (message
   (format "Start up in %.2fs with %d features and %d GC(s)"
	   (float-time (time-subtract after-init-time before-init-time))
	   (length features)
	   gcs-done)))
(add-hook 'after-init-hook #'self/print-init-info)

(provide 'init)
;;; init.el ends here
