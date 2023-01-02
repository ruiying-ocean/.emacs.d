;;; init.el --- Emacs configuration -*- lexical-binding: t -*-

;; This file contains my customized configuration codes, which
;; are divided into multiple sections by ^L character.
;; Author: Rui Ying
;; Email: rui.ying@bristol.ac.uk

;;; DEPENDENCIES
;; LSP servers:
;;       pylsp, clangd, fortls, texlab/digestif
;; Spell checker:
;;       grammaly
;; Lint checker:
;;       pyflakes, shell checker (brew)
;; Fonts:
;;       all-the-icons, Roboto Mono, Iosevka, SF Mono
;; Others:
;;       ripgrep, libvterm, PDF tools, multimarkdown (brew),
;;       npm package `livedown`


;;; Code:

;;; FUNDEMENTAL SETUP

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
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Intergration with use-package
(straight-use-package 'use-package)
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
  (dolist (dir '("extra-lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))
(advice-add #'package-initialize :after #'update-load-path)
(update-load-path)

(when (eq system-type 'darwin)
  (defvar brew-parent-dir "/opt/homebrew/")
  (defvar brew-bin-dir (expand-file-name "bin/" brew-parent-dir))
  (defvar emacs-path "/opt/homebrew/bin/emacs"))

;; Avoid matching file name with regrex list during startup
(let ((file-name-handler-alist nil)) "~/.emacs.d/init.el")

;; Custom file
(setq-default custom-file (concat user-emacs-directory "extra-lisp/custom.el"))
(load custom-file :noerror)

;; Benchmark init time
(use-package esup
  :custom
  (esup-depth 0))

;; restart emacs
(use-package restart-emacs)

;;; EDITOR

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

;;No more backup files~
(setq-default make-backup-files nil)

;;No more strange ring bell
(setq ring-bell-function 'ignore)

(setq confirm-kill-processes t)

;; Delete selection
(delete-selection-mode t)

;; (global-set-key (kbd "M-w ") 'duplicate-dwim)

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
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  :bind
  ("C-c u" . undo-tree-visualize)
  ("M-/" . undo-tree-redo)
  ("C-/" . undo-tree-undo))

;; undo and redo changes in the *window configuration*
(use-package winner
  :straight (:type built-in)
  :hook
  (after-init . winner-mode)
  :bind
  (:map winner-mode-map
	("C-M-b" . winner-undo)
	("C-M-f" . winner-redo)))

;; move within buffers
(use-package mosey
  :bind
  ("C-a" . mosey-backward-bounce)
  ("C-e" . mosey-forward-bounce)
  :config
  (defmosey '(beginning-of-line
	      back-to-indentation
	      sp-backward-sexp		; Moves across lines
	      sp-forward-sexp		; Moves across lines
	      mosey-goto-end-of-code
	      mosey-goto-beginning-of-comment-text)
    :prefix "lisp"))

;;; Auto-save buffer
(use-package real-auto-save
  :hook
  (prog-mode . real-auto-save-mode)
  (text-mode . real-auto-save-mode)
  :custom
  ;; configure time gap (in sec)
  (real-auto-save-interval 1))

;; auto select and cut/copy
;; manual control: press another `w` (word),
;; `s` (expression), `l` (list), `f` (file)
;; `d` (defun), '+', '-'
(use-package easy-kill
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill)
  (global-set-key [remap mark-sexp] 'easy-mark))

;; Smartly clean whitespace
;; (use-package whitespace-cleanup-mode
;;   :hook
;;   (prog-mode . whitespace-mode))

;; locally remove trailing whitespace for programming mode
;; (add-hook 'prog-mode-hook
;;           (lambda () (add-hook 'before-save-hook 'delete-trailing-whitespace nil 'local)))

;; An alternative way to cleanup whitespace
;; (use-package ws-butler
;;   :straight (:host github :repo "lewang/ws-butler")
;;   :hook
;;   (prog-mode . ws-butler-mode))

;; Automatically add spacing around operators
;; use C-v to next page
(use-package electric-operator
  :hook
  ;; (python-mode . electric-operator-mode)
  ;; (emacs-lisp-mode . electric-operator-mode)
  (ess-r-mode . electric-operator-mode))

(use-package smart-newline
  :straight (:host github :repo "ainame/smart-newline.el")
  :config
  (smart-newline-mode 1)
  :bind
  ("C-j" . smart-newline))

;; smartly select region, press until it selects what you want
;; C-M-SPC also does same on Mac
(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

;; assign every marked line a cursor
(use-package multiple-cursors
  :init
  (global-unset-key (kbd "M-<down-mouse-1>"))
  :bind
  ("C-M-<down>" . mc/mark-next-like-this)
  ("C-M-<up>" . mc/mark-previous-like-this)
  ("M-<mouse-1>" . mc/add-cursor-on-click))

(use-package ediff
  :defer 1
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
  :bind
  ("C-x s" . shell)
  (:map shell-mode-map
	("C-k" . comint-kill-whole-line)
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
  (vterm-max-scrollback 100000))

;; a comint extension, e.g., :view *.jpg to view a plot in shell
;; other useful cmd: :e (edit), :ssh,
(use-package shx
  :hook
  (shell-mode . shx-global-mode))

;;To specify new version of git on remote machine so I can run magit locally
;;add ~/.ssh/config and ~/.ssh/known_hosts first
;;then ssh-keygen -t rsa => ssh-copy-id name@host_name
;; .inputrc file may trigger bug of "timeout reached"
(use-package tramp
  :defer 1
  :straight (:type built-in)
  :if (memq system-type '(gnu/linux darwin))
  :config
  ;; use for debug
  ;; (setq tramp-verbose 6)
  (add-to-list 'tramp-remote-path "/usr/local/bin/git")
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  ;;tramp mode to cache password
  (setq password-cache-expiry nil)
  ;; version control backends
  (setq vc-handled-backends '(Git))
  )

(use-package which-key
  :hook
  (after-init . which-key-mode))

;; on-the-fly syntax checker,  use C-c ! as prefix, e.g., C-c ! v to verify the checkers
;; use M-g n/p to navigate error, or use C-c e (counsel-flycheck)

;; python flycheck depdencies
;; pip install pyflakes -> fast and don't check code style

(use-package flycheck-pyflakes
  :after python)

;; shell -> shell-checker
;; python -> pyflakes
;; R -> disabled
;; Elisp -> default
(use-package flycheck
  :hook
  (prog-mode . flycheck-mode)
  :config
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  (setq-default flycheck-disabled-checkers '(sh-posix-dash
					     sh-posix-bash
					     python-flake8
					     python-pylint
					     python-mypy
					     python-pyright
					     python-pycompile
					     emacs-lisp-checkdoc)))

(use-package flycheck-inline
  :hook
  (flycheck-mode . flycheck-inline-mode))

(use-package flycheck-grammarly
  :config
  ;; (with-eval-after-load 'flycheck
  ;;   (flycheck-grammarly-setup))
  (setq flycheck-grammarly-check-time 0.8)
  :hook
  (flycheck-mode . flycheck-grammarly-setup))

;; A dictionary inside Emacs, by abo-abo!
(use-package define-word
  :bind
  ("C-c d" . define-word-at-point)
  :config
  (setq define-word-default-service 'webster))

;; insert template
;; change trigger key!
;; (use-package yasnippet
;;   :straight yasnippet-snippets ;; Collection of snippets
;;   :hook
;;   (after-init . yas-global-mode)
;;   :bind
;;   ("C-c i" . yas-insert-snippet))

(use-package consult-yasnippet
  :after (consult yasnippet))

;;Git + Emacs = boom!
(use-package magit
  :bind
  ("C-x g" . magit-status)
  ("C-x c" . magit-checkout)
  :config
  ;; for verbose
  (setq magit-refresh-verbose t))

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
  :defer t
  :after exec-path-from-shell) ;;extend use-package, put after exec-path-from-shell

;; pop up window management
(use-package popwin
  :hook
  (after-init . popwin-mode))

;; better isearch, choose this or consult-line
(use-package ctrlf
  :hook
  (after-init . ctrlf-mode)
  :config
  (setq ctrlf-default-search-style 'fuzzy))

;; completion UI
(use-package vertico
  :hook
  (after-init . vertico-mode))

;; displays current match and total matches in search
(use-package anzu
  :hook
  (prog-mode . global-anzu-mode))

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

;; completion strategy
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; rich annotations of minibuffer
(use-package marginalia
  ;; change more or less info
  :bind (:map minibuffer-local-map
	      ("M-A" . marginalia-cycle))
  :hook
  (after-init . marginalia-mode))

;; auto completion
(use-package corfu
  :hook
  (after-init . global-corfu-mode)
  :custom
  (corfu-auto nil)
  (corfu-auto-delay 0.75)
  (corfu-preview-current t)
  :bind
  (:map corfu-map
	("C-n" . corfu-next)
	("C-p" . corfu-previous)
	("C-m" . corfu-insert)
	("<return>" . corfu-insert)
	("RET" . corfu-insert)
	("C-M-i" . corfu-complete)))


;; documentation
(use-package corfu-doc
  :straight (:type git :host github
		   :repo "galeo/corfu-doc")
  :hook
  (corfu-mode . corfu-doc-mode))

;; icon like all-the-icons
(use-package kind-icon
  :straight (:type git :host github
		   :repo "jdtsmith/kind-icon")
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package emojify
  :config
  (when (member "Segoe UI Emoji" (font-family-list))
    (set-fontset-font
     t 'symbol (font-spec :family "Segoe UI Emoji") nil 'prepend))
  (setq emojify-display-style 'unicode)
  (setq emojify-emoji-styles '(unicode))
  :bind
  ("C-c ." . emojify-insert-emoji))

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

(use-package embark
  :bind
  (("<f6>" . embark-act)	;; pick some comfortable binding
   ("C-;" . embark-dwim)	;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


;; jump to definition
(use-package dumb-jump
  :config
  (setq dumb-jump-prefer-searcher 'rg)
  ;; xref as backend
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  ;; customized xref to use `completing-read' to select a target
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read))

;; a bunch of advanced commands: buffer switching, imenu, search commands etc.
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
	 ("C-c h" . consult-history)
	 ("C-c m" . consult-mode-command)

	 ;; C-x bindings (ctl-x-map)
	 ("C-x b" . consult-buffer)	;; orig. switch-to-buffer
	 ("C-x r b" . consult-bookmark) ;; orig. bookmark-jump

	 ;; Other custom bindings
	 ("M-y" . consult-yank-pop)	;; orig. yank-pop
	 ("<help> a" . consult-apropos) ;; orig. apropos-command

	 ;; M-g bindings (goto-map)
	 ("M-g e" . consult-compile-error)
	 ;; ("M-g f" . consult-flycheck) ;; Alternative: consult-flycheck
	 ("M-g g" . consult-goto-line)	 ;; orig. goto-line
	 ("M-g M-g" . consult-goto-line) ;; orig. goto-line
	 ("M-g o" . consult-outline) ;; Alternative: consult-org-heading
	 ("M-g m" . consult-mark)
	 ("M-g k" . consult-global-mark)
	 ("M-g i" . consult-imenu)
	 ("M-g I" . consult-imenu-multi)

	 ;; M-s bindings (search-map)
	 ;; ("M-s l" . consult-line)
	 ;; use consult-line to replac
	 ("C-s" . consult-line)
	 ("M-s f" . affe-find)
	 ("M-s s" . affe-grep)
	 ("M-s r" . consult-ripgrep)
	 ("M-s L" . consult-line-multi)
	 ("M-s m" . consult-multi-occur)
	 ("M-s k" . consult-keep-lines)
	 ("M-s u" . consult-focus-lines)
	 ("<f3>" . consult-recent-file)
	 ("M-s e" . consult-isearch-history)
	 :map isearch-mode-map
	 ("M-e" . consult-isearch-history) ;; orig. isearch-edit-string
	 ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
	 ("M-s l" . consult-line) ;; needed by consult-line to detect isearch
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


;; asynchronous fuzzy find/grep (with just 200+ lines)
;; used for project search
(use-package affe
  :defer 0.5)

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
  ("C-\"" . avy-goto-char)		;input: one character
  ("C-'" . avy-goto-char-2)		;input: two characters
  ("M-g w" . avy-goto-word-1)
  ("M-g l" . avy-goto-line))

(use-package ace-window
  :bind
  ("M-o" . ace-window)
  ("C-x o" . ace-swap-window))


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

(use-package goto-last-change
  :bind
  ("C-x C-x" . goto-last-change))

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

(defun open-init-file()
  "Open my init.el."
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "<f2>") 'open-init-file)

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
  (rime-emacs-module-header-root "/opt/homebrew/Cellar/emacs-plus@29/29.0.60/Emacs.app/Contents/Resources/include/")
  (rime-librime-root "~/.emacs.d/librime/dist")
  )

;; history across buffers
(use-package dogears
  :straight (:host github :repo "alphapapa/dogears.el")
  ;; These bindings are optional, of course:
  :bind (:map global-map
	      ("M-g d" . dogears-go)
	      ("M-g b" . dogears-back)
	      ("M-g f" . dogears-forward)
	      ("M-g D" . dogears-list)))

;; Mark set
;; C-x C-x -> set mark and move back to previous position
;; C-x h to select all

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

    "g l" '(avy-goto-line :which-key "goto-line")
    "g g" '(goto-line :which-key "goto-line-number")
    "g m" '(exchange-point-and-mark :which-key "go-back-and-mark")
    ;; "g b" '(goto-last-change :which-key "go-back")
    "g b" '(dogears-back :which-key "go-back")

    "m" '(indent-rigidly :which-key "move code")

    "p f" '(affe-find :which-key "project find file")
    "p s" '(affe-grep :which-key "project search text")
    "p b" '(consult-projectile :which-key "project buffer/file")
    "p c" '(projectile-compile-project :which-key "project compile")
    "p p" '(projectile-switch-project :which-key "project switch")
    "p v" '(projectile-run-vterm :which-key "project vterm")
    "p x" '(projectile-run-shell :which-key "project shell")
    "p e" '(projectile-run-eshell :which-key "project eshell")

    "e b" '(ediff-buffers :which-key "compare buffers")
    "e f" '(ediff-files :which-key "compare files")

    "h a" '(mark-whole-buffer :which-key "select all")

    "." 'mc/mark-next-like-this
    "," 'mc/mark-previous-like-this

    "1" 'beginning-of-buffer
    "2" 'end-of-buffer

    "v" 'vterm
    "s" 'shell

    "f" 'find-file
    "k" 'kill-this-buffer
    "q" 'save-buffers-kill-terminal)

  ;; ------ Mode-specific Keybindings ------
  (my/leader-def prog-mode-map
    "b" 'consult-imenu
    "s" 'shell
    "t" 'vterm
    "d" 'ediff-buffers
    "c" 'consult-flycheck
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

;; folding your code
(use-package origami
  :bind
  ("<f5>" . origami-toggle-node))

;; adjust font size
(setq-default text-scale-mode-step 1.1)

(use-package helpful
  :config
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable)
  :bind
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  ("C-h d" . helpful-at-point)
  ("C-h C" . helpful-command)
  ("C-h F" . helpful-function))

;; find and replace
(global-set-key (kbd "C-c %") 'query-replace)
(global-set-key (kbd "C-c R") 'query-replace-regexp)

;; select one and edit all (https://github.com/victorhge/iedit)
;; iedit is also dependency of lispy, use M-i to toggle
(use-package iedit
  :bind
  ("M-i" . iedit-mode))

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
  :straight (:host github
		   :repo "Malabarba/beacon")
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

;;Disable line number for certain modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook
		LaTeX-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; code formatting, require third-party formatter
(use-package format-all
  :hook
  (prog-mode . format-all-mode))

;; built-int choices:
;; (electric-indent-mode 1)

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
  (after-init . pulsing-cursor-mode))

;;--------------------------------------------------
;; Matching parenthesis
;;--------------------------------------------------

;; Showing matching parentheses (built-in)
;; (show-paren-mode t)
;; (setq show-paren-delay 0)
;; (setq show-paren-style 'parenthesis) ;;Options: parenthesis/expression/mixed

(use-package highlight-parentheses
  :hook
  (after-init . global-highlight-parentheses-mode)
  :config
  (setq highlight-parentheses-highlight-adjacent t)
  ;;  (setq highlight-parentheses-colors '("BlueViolet" "DarkOrchid" "orchid" "Plum"))
  )

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

(use-package highlight-symbol
  ;; An alternative package is highlight-thing
  :bind
  ("C-<f9>" . highlight-symbol)
  ("<f9>" . highlight-symbol-next)
  ("S-<f9>" . highlight-symbol-prev)
  ("M-<f9>" . highlight-symbol-query-replace))

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

;;English font: Iosevka/Inconsolata/Juliamono/Jetbrains Mono/Roboto Mono/Monaco/Fira Code/SF Mono/Operator Mono
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
	(set-face-attribute 'default nil :font (format "%s:pixelsize=%d" "Iosevka" 15))
	;; CJK font
	(dolist (charset '(kana han symbol cjk-misc bopomofo))
	  (set-fontset-font (frame-parameter nil 'font)
			    charset
			    (font-spec :family "LXWG WenKai"))))))

;; Use emacs daemon, put following lines to shell config file
;; alias emacs=/path_2_miniconda3/bin/emacs
;; alias emacsclient=/path_2_miniconda/bin/emacsclient
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

;; use modus-operandi as default theme
(use-package modus-themes
  :config
  ;; mode line, style, padding & height
  (setq modus-themes-mode-line '(accented borderless))
  ;; syntax style
  (setq modus-themes-syntax '(alt-syntax green-strings yellow-comments))
  (setq modus-themes-italic-constructs t
	modus-themes-bold-constructs nil
	modus-themes-region '(accented bg-only no-extend))
  (setq modus-themes-links '(neutral-underline background))
  ;; finally load the theme
  (load-theme 'modus-operandi))

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


;;; PROGRAMMING LANGUAGES & LSP
;;==============================
;; LSP-bridge
(add-to-list 'load-path "~/.emacs.d/lsp-bridge")
(require 'lsp-bridge)
(global-lsp-bridge-mode)
(setq acm-enable-icon t)
(setq lsp-bridge-complete-manually nil)
(define-key lsp-bridge-mode-map (kbd "<return>") 'acm-complete)
(setq lsp-bridge-enable-auto-format-code nil)
(setq acm-enable-tabnine t)

(add-to-list 'load-path "~/.emacs.d/color-rg")
(require 'color-rg)

;; requires install `sbcl`
;; REPL for lisp
;; (use-package slime
;;   :config
;;   (setq inferior-lisp-program "sbcl"))

(use-package stan-mode
  :mode ("\\.stan\\'" . stan-mode)
  :hook (stan-mode . stan-mode-setup)
  ;;
  :config
  ;; The officially recommended offset is 2.
  (setq stan-indentation-offset 2))

;; sticky header of code block
(use-package topsy
  :straight (topsy :fetcher github :repo "alphapapa/topsy.el")
  :hook (prog-mode . topsy-mode))

;; (1) brew install treesit
;; (2) build extra treesit language definition
(use-package treesit
  :if (treesit-available-p)
  :config
  ;; treesit language definition
  (setq treesit-extra-load-path '("/usr/local/lib/tree-sitter-module/dist"))
  ;; use  python-ts(i.e., treesit)-mode
  (if (treesit-ready-p 'python)
      (push '(python-mode . python-ts-mode) major-mode-remap-alist)))

;;==============================
;;           Python           ;;
;;==============================

;; Interpreter choice
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True")
;;(setq python-shell-interpreter "python3.9")

;;python-style indent
(setq python-indent-offset 4)
(setq python-indent-guess-indent-offset-verbose nil) ;;don't complain about the indent anymore
(setq python-indent-guess-indent nil)
(setq indent-tabs-mode t) ;; whether tabs are used for indentation

;;debug setting
(setq python-shell-completion-native-enable nil) ;;or pip3 install pyreadline to avoid warning
(setq python-shell-prompt-detect-failure-warning nil)
(setq python-shell-enable-font-lock nil) ;;make printing fast

;;A dirty solution of showing inferior-python input codes
;;from https://github.com/jorgenschaefer/elpy/issues/924
(defun python-shell-append-to-output (string)
  (let ((buffer (current-buffer)))
    (set-buffer (process-buffer (python-shell-get-process)))
    (let ((oldpoint (point)))
      (goto-char (process-mark (python-shell-get-process)))
      (insert string)
      (set-marker (process-mark (python-shell-get-process)) (point))
      (goto-char oldpoint))
    (set-buffer buffer)))

(defadvice python-shell-send-string
    (around advice-python-shell-send-string activate)
  (interactive)
  (let* ((append-string1
          (if (string-match "import codecs, os;__pyfile = codecs.open.*$" string)
              (replace-match "" nil nil string)
            string))
         (append-string2
          (if (string-match "^# -\\*- coding: utf-8 -\\*-\n*$" append-string1)
              (replace-match "" nil nil append-string1)
            append-string1))
         (append-string
          (if (string-match "^\n*$" append-string2)
              (replace-match "" nil nil append-string2)
            append-string2)))
    (python-shell-append-to-output
     (concat (string-trim-right append-string) "\n")))
  (if (called-interactively-p 'any)
      (call-interactively (ad-get-orig-definition 'python-shell-send-string))
    ad-do-it))

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

;; use jupyter text to pair format
;; jupytext --set-formats ipynb,py notebook.ipynb
;; jupytext --sync notebook.ipynb
(use-package conda
  :config
  (conda-env-initialize-interactive-shells)
  (cond ((file-directory-p "~/miniconda3")
	 (setq conda-anaconda-home (expand-file-name "~/miniconda3")
	       conda-env-home-directory (expand-file-name "~/miniconda3")))
	((file-directory-p "~/miniforge3")
	 (setq conda-anaconda-home (expand-file-name "~/miniforge3")
	       conda-env-home-directory (expand-file-name "~/miniforge3")))
	(((eq system-type 'darwin))
	 (setq conda-anaconda-home "/opt/homebrew/Caskroom/miniforge"
	       conda-env-home-directory "/opt/homebrew/Caskroom/miniforge")))

  ;; when in conda-project-env-name or has environmental.yml auto activate
  (conda-env-autoactivate-mode t)
  :bind
  ("C-c c a" . conda-env-activate)
  ("C-c c d" . conda-env-deactivate))

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

  ;; disable flycheck because lsp has linter already
  (add-hook 'ess-r-mode-hook (lambda () (flycheck-mode -1)))

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

;; view R data frame
;; https://github.com/ShuguangSun/ess-view-data
(use-package ess-view-data
  :defer t)

;;C-c C-a to turn on csv-align-fields
(use-package csv-mode
  :mode
  "\\.csv\\'"
  "\\.CSV\\'")

;;display color of RGB code
(use-package rainbow-mode
  :hook
  (ess-r-mode . rainbow-mode)
  (js-mode . rainbow-mode))

;;;;;;;;;;;;
;; Matlab ;;
;;;;;;;;;;;;

;; cd /path/to/matlab-emacs -> make
;; Homepage: https://sourceforge.net/p/matlab-emacs/src/ci/documentation/tree/
(use-package matlab-mode
  :defer t
  :mode "\\.[mM]\\'")

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

;; Beautify org-mode
(use-package org-superstar
  :hook
  (org-mode . org-superstar-mode))

(use-package svg-tag-mode
  :init
  (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
  (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
  (defconst day-re "[A-Za-z]\\{3\\}")
  (defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))

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

  (setq svg-tag-tags
	`(
	  ;; Org tags
	  (":\\([A-Za-z0-9]+\\)" . ((lambda (tag) (svg-tag-make tag))))
	  (":\\([A-Za-z0-9]+[ \-]\\)" . ((lambda (tag) tag)))

	  ;; Task priority
	  ("\\[#[A-Z]\\]" . ((lambda (tag)
			       (svg-tag-make tag :face 'org-priority
					     :beg 2 :end -1 :margin 0))))

	  ;; Progress
	  ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
					      (svg-progress-percent (substring tag 1 -2)))))
	  ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
					    (svg-progress-count (substring tag 1 -1)))))

	  ;; TODO / DONE
	  ("TODO" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-todo :inverse t :margin 0))))
	  ("DONE" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :margin 0))))


	  ;; Citation of the form [cite:@Knuth:1984]
	  ("\\(\\[cite:@[A-Za-z]+:\\)" . ((lambda (tag)
					    (svg-tag-make tag
							  :inverse t
							  :beg 7 :end -1
							  :crop-right t))))
	  ("\\[cite:@[A-Za-z]+:\\([0-9]+\\]\\)" . ((lambda (tag)
						     (svg-tag-make tag
								   :end -1
								   :crop-left t))))


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
	      (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date))))))

  :hook org-mode)

;; pretty symbol for org-mode
(add-hook 'org-mode-hook
	  (lambda ()
	    ;; tickboxes
	    (push '("[ ]" . "🞎") prettify-symbols-alist)
	    (push '("[X]" . "☑") prettify-symbols-alist)
	    (push '("[-]" . "◫") prettify-symbols-alist)
	    ;; arrows
	    (push '("->" . "→") prettify-symbols-alist)
	    (push '("<-" . "←") prettify-symbols-alist)
	    (push '("=>" . "⇒") prettify-symbols-alist)
	    (push '("<=" . "⇐") prettify-symbols-alist)
	    (push '("\\->" . "↳") prettify-symbols-alist)
	    (push '("<-/" . "↵") prettify-symbols-alist)
	    (prettify-symbols-mode)))

(setq org-todo-keywords
      '((sequence "TODO(t)" "DOING(s)" "|" "DONE(d!/!)")))

;; Perfectly alian English/CJK fonts in the same table
(use-package valign
  :hook (org-mode . valign-mode)
  :config
  (setq valign-fancy-bar t))

;; capture TODOs
;; %U -> dat; %i -> current selection;
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Documents/TODO.org" "Tasks")
	 "* TODO [#A] %? %i %U"
	 :empty-lines 1)))

(setq org-directory "~/Documents")
(setq org-default-notes-file (concat org-directory "/TODO.org"))
(global-set-key (kbd "C-c r") 'org-capture)

;; ==============================
;;             LATEX           ;;
;; ==============================
;; First, add TeX distribution to path
(use-package tex ;;not auctex instead!
  :straight auctex
  :mode ("\\.tex\\'" . latex-mode)
  ;; one command to compile and view
  :bind (:map latex-mode-map
	      ("C-c C-a" . Tex-command-run-all))
  :hook
  (LaTeX-mode . rainbow-delimiters-mode)
  (LaTeX-mode . LaTeX-math-mode)
  (LaTeX-mode . visual-line-mode)
  (LaTeX-mode . flycheck-mode)
  ;; sync TeX and PDF
  (LaTeX-mode . TeX-source-correlate-mode)

  :config
  ;; enable document parsing
  (setq TeX-auto-save t
	TeX-parse-self t)
  ;; tex directory structure
  (setq-default TeX-master nil)
  (setq-default TeX-engine 'latex) ;;default engine
  (setq-default TeX-PDF-mode t)	   ;;PDF output
  (setq-default TeX-master nil)

  ;;sync latex <-> pdf
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer) ;;auto revert PDF buffer

  ;;C-c C-v to sync forward, double click to sync backward
  (setq TeX-view-program-selection '((output-pdf "PDF Tools")
				     (output-dvi "DVI Viewer"))
	TeX-source-correlate-start-server t
	TeX-source-correlate-method 'auto) ;;Method to use for enabling forward and inverse search
  )

(use-package magic-latex-buffer
  :hook
  (LaTeX-mode . magic-latex-buffer)
  :config
  (setq magic-latex-enable-block-highlight nil
	magic-latex-enable-suscript t
	magic-latex-enable-pretty-symbols t
	magic-latex-enable-block-align nil
	magic-latex-enable-inline-image nil
	magic-latex-enable-minibuffer-echo nil))

;;; PDF READER

;;allow you to view pdf continuously
(use-package pdf-continuous-scroll-mode
  :straight (:host github :repo "dalanicolai/pdf-continuous-scroll-mode.el")
  :hook
  (pdf-view-mode-hook . pdf-continuous-scroll-mode))

;; Compile and install: https://github.com/politza/pdf-tools
;; Install dependencies: cask, poppler, automake and setenv
;; Download source code and compile
;; (pdf-tools-install)
(use-package pdf-tools
  :commands (pdf-view-mode pdf-loader-install)
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query)
  (define-pdf-cache-function pagelables)
  ;;In case of high-resolution screen like Mac
  (setq pdf-view-use-scaling t
	pdf-view-use-imagemagick nil)
  :hook
  (pdf-view-mode . (lambda ()
		     (display-line-numbers-mode -1)))
  (pdf-view-mode . pdf-tools-enable-minor-modes)
  (pdf-view-mode . pdf-view-themed-minor-mode)
  :bind
  (:map pdf-view-mode-map
	("C-s" . isearch-forward-regexp)
	("j" . pdf-view-next-line-or-next-page)
	("k" . pdf-view-previous-line-or-previous-page)))


;;; Final cook up

;; Back to normal GC level
(defun my-cleanup-gc ()
  "Clean up gc.  From user redguardtoo."
  (setq gc-cons-threshold 100000000)
  (setq gc-cons-percentage 0.1))

;; Collect gc during free time
(run-with-idle-timer 4 nil #'my-cleanup-gc)

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
