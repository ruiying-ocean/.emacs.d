;;; init.el --- Load the full configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;; Always remember that it is WHAT YOU ARE EDITING, rather
;; than the editor, that is the key.
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

;; This file contains customized configuration codes, which
;; have been divided into multiple sections by ^L character.

;;; EXTERNAL DEPENDENCIES:
;; LSP servers: pylsp, clangd, fortls, texlab/digestif
;; Spell checker: languagetool, aspell/huspell
;; Lint checker: python-flakes, shell checker
;; Fonts: all-the-icons, Roboto Mono, Iosevka, SF Mono
;; Others: ripgrep, fzf, libvterm, PDF tools, multidown,
;; github-token for grip-mode, Math preview for LaTeX inline preview

;;; Code:


;;; FUNDEMENTAL

;; Package Manager: straight.el
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

;; Customize when to check package modification
(setq straight-check-for-modifications '(check-on-save))
;; Cause straight.el to cache the autoloads of all used packages in a single
;; file on disk thus reduce IO operations
(setq straight-cache-autoloads t)

;; Intergration with use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Emacs Native Compilation Feature support
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (progn
    (setq native-comp-async-report-warnings-errors nil)
    (setq comp-deferred-compilation t)
    (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))
    (setq package-native-compile t)
    ))

;; Update user load path
;; Optimize: Force "lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("extra-lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))
(advice-add #'package-initialize :after #'update-load-path)
(update-load-path)

;; Custom file
(setq custom-file (concat user-emacs-directory "/extra-lisp/custom.el"))
(load custom-file :noerror)

;;; Speed up launching Emacs

;; Avoid matching file name with regrex list during startup
(let ((file-name-handler-alist nil)) "~/.emacs.d/init.el")

;; Another package to automatically optimise garbage collector
;; (use-package gcmh
;;   :straight (gcmh :type git :host gitlab :repo "koral/gcmh")
;;   :config
;;   (gcmh-mode +1)
;;   (setq gcmh-verbose t))

;; Recompile outdated .elc file
;; (use-package auto-compile
;;   :init
;;   (auto-compile-on-load-mode)
;;   (auto-compile-on-save-mode))

;; Benchmark init time
;; Option 1
;; `time emacs -e kill-emacs`

;; Option 2
;; (setq use-package-verbose t)
;; (use-package-statistic-mode)

;; Option 3
;; (use-package benchmark-init
;;   :config
;;   ;; To disable collection of benchmark data after init is done.
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; Option 4
(use-package esup
  :config
  (setq esup-depth 0))

;; Option 5 (Run time not start up)
;; profile-start -> profile-stop -> profile-report


;;; EDITOR SECTION

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

;;We are lazy human :)
(fset 'yes-or-no-p 'y-or-n-p)

;;No more backup files~
(setq-default make-backup-files nil)

;;No more strange ring bell
(setq ring-bell-function 'ignore)

;; Delete selection
(delete-selection-mode t)

;; Chinese input method, would be great for inconvenient environment
;; (use-package pyim
;;   :unless (window-system)
;;   :config
;;   (require 'liberime nil t)
;;   (pyim-default-scheme 'rime-quanpin)
;;   ;; (setq default-input-method "pyim")
;;   (if (window-system)
;;       (setq pyim-page-tooltip 'posframe)
;;     (setq pyim-page-tooltip 'popup))
;;   (setq pyim-page-length 7)		; list of candidate
;;   (setq ivy-re-builders-alist
;; 	'((t . pyim-cregexp-ivy))))

;; (use-package pyim-greatdict
;;   :straight (:type git :host github
;; 		   :repo "tumashu/pyim-greatdict")
;;   :after pyim
;;   :config
;;   (pyim-greatdict-enable))

;; repeat command
(global-set-key (kbd "<f4>") #'repeat)

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

(use-package so-long
  :straight (:type built-in)
  :hook
  (after-init . global-so-long-mode))

;; auto revert buffer
(use-package autorevert
  :straight (:type built-in)
  :hook
  (after-init . global-auto-revert-mode))

;; Display next page at the other window
;; (setq follow-mode t)

;;;; Enable mouse operation in terminal emacs
(unless (display-graphic-p)
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

;; Hack clipboard for macOS in TUI mode
(defun my/kill-ring-save (orig-fun beg end &optional region)
  (unless (display-graphic-p)
    (let ((inhibit-message t))
      (shell-command-on-region beg end "pbcopy")))
  (funcall orig-fun beg end region))
(advice-add 'kill-ring-save :around #'my/kill-ring-save)

(use-package languagetool
  :config
  (setq languagetool-language-tool-jar
	(concat (getenv "HOME") "/LanguageTool-5.4/languagetool-commandline.jar"))
  (setq languagetool-language-tool-server-jar
	(concat (getenv "HOME") "/LanguageTool-5.4/languagetool-server.jar"))
  (setq languagetool-server-user-arguments '("-p" "8082"))
  (setq languagetool-default-language "en-GB")
  (setq languagetool-java-bin "/usr/bin/java")
  (setq languagetool-java-arguments '("-Dfile.encoding=UTF-8"))
  :bind
  ("C-c C-; c" . languagetool-check)
  ("C-c C-; d" . languagetool-clear-buffer)
  ("C-c C-; i" . languagetool-correct-at-point)
  ("C-c C-; b" . languagetool-buffer)
  ("C-c C-; l" . languagetool-set-language))

;; A dictionary inside Emacs, by abo-abo!
(use-package define-word
  :bind
  ("C-c d" . define-word-at-point)
  :config
  (setq define-word-default-service 'webster))

;;flyspell setting
(use-package flyspell
  :straight (:type built-in)
  :hook
  (text-mode . flyspell-mode)
  (org-mode . flyspell-mode)
  (LaTeX-mode . flyspell-mode)
  :config
  (setq-default ispell-program-name "aspell") ;;depends on aspell in the path
  (setq ispell-local-dictionary "en_GB")
  (setq ispell-extra-args '("--sug-mode=fast" "--lang=en_GB"
			    "--camel-case" "--run-together")))

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

;; Flyspell interface
;; Use M-o to do words action (e.g., save)
(use-package flyspell-correct-ivy
  :after flyspell-correct)

;; ---Edit keybinding style---
;; >>> OPTION1: evil (vim-like)
;; (use-package evil
;;   :hook
;;   (after-init . evil-mode)
;;   :config
;;   (setq evil-disable-insert-state-bindings t))

;; (use-package evil-collection
;;   :custom
;;   (evil-collection-setup-minibuffer t)
;;   (evil-want-keybinding nil)
;;   :init
;;   (evil-collection-init))
;; >>> OPTION2: viper-mode (built-in vim-like)
;; (use-package viper
;;   :straight (:type built-in)
;;   :init (setq viper-mode t)
;;   )

;; >>> OPTION3: god-mode (remove prefix key)
;; (use-package god-mode
;;   :init
;;   (god-mode)
;;   )

(use-package recentf
  :straight (:type built-in)
  :config
  (setq-default
   recentf-max-saved-items 30
   recentf-exclude `("/tmp/",
		     (concat "~/.emacs.d/straight/build" "/.*-autoloads\\.el\\'")))
  (global-set-key (kbd "<f3>") #'recentf-open-files)
  :hook
  (after-init . recentf-mode))

;;use undo-tree-visualize to show history
(use-package undo-tree
  :hook
  (after-init . global-undo-tree-mode)
  :bind
  ("C-c u" . undo-tree-visualize))

;;; Auto-save
(use-package super-save
  :config
  (super-save-mode 1)
  ;; turn off the buil-in auto-save
  (setq auto-save-default nil)
  (add-to-list 'super-save-triggers 'ace-window)
  (add-to-list 'super-save-hook-triggers 'find-file-hook)
  (setq super-save-remote-files nil)
  (setq super-save-exclude '(".gpg")))

;; A replacement to buil-tin M-w, now you can
;; save word/sexp/list/defun/file by M-w w/s/l/d/f
(use-package easy-kill
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill)
  (global-set-key [remap mark-sexp] 'easy-mark))

;; Smartly clean whitespace
;; (use-package whitespace-cleanup-mode
;;   :hook
;;   (prog-mode . whitespace-mode))

;; locally remove trailing whitespace for programming mode
(add-hook 'prog-mode-hook
          (lambda () (add-hook 'before-save-hook 'delete-trailing-whitespace nil 'local)))

;; An alternative way to cleanup whitespace
(use-package ws-butler
  :straight (:type git :host github
		   :repo "lewang/ws-butler")
  :hook
  (prog-mode . ws-butler-mode))

;; Automatically add spacing around operators
(use-package electric-operator
  :hook
  ;; (python-mode . electric-operator-mode)
  ;; (emacs-lisp-mode . electric-operator-mode)
  (ess-r-mode . electric-operator-mode))

(use-package smart-newline
  :straight (:type git :host github
		   :repo "ainame/smart-newline.el")
  :config
  (smart-newline-mode 1)
  :bind
  ("C-j" . smart-newline))

;; smartly select region, press until it selects what you want
(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

;; assign every marked line a cursor
(use-package multiple-cursors
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C-x >" . mc/mark-next-like-this)
  ("C-x <" . mc/mark-previous-like-this)
  ("C-x C-<" . mc/mark-all-like-this))


;;; TERMINAL, COMPLETION, LINT, SNIPPET

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
	("<up>" . comint-previous-input)
	("C-p" . comint-previous-input)
	("<down>" . comint-next-input)
	("C-n" . comint-next-input)
	("C-l" . comint-clear-buffer)
	("SPC" . comint-magic-space)))     ;magically expand history reference

;; Enable this to get a superior terminal emulator (a true application like iTerm)
;; read more on https://github.com/akermu/emacs-libvterm to see the external dependencies
(use-package vterm
  :bind
  ("C-x t" . vterm))

;; get better rendering experience for term/ansi-term
(use-package eterm-256color
  :hook
  (term-mode . eterm-256color-mode))

;; a comint extension, e.g., :view *.jpg to view a plot in shell
;; other useful cmd: :e (edit), :ssh,
(use-package shx
  :hook
  (after-init . shx-global-mode))

;;auto-completion system
(use-package company
  :config
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
  (setq company-global-modes '(not inferior-python-mode))
  (setq company-idle-delay 0.8)
  (setq company-show-numbers t)
  :hook
  (after-init . global-company-mode))

;; company for shell script
(use-package company-shell
  :config
  (add-to-list 'company-backends 'company-shell-env))

;;A machine-learning based backend for company
;;May conflict with company-flx-mode/ESS mode
;; (use-package company-tabnine
;;   :defer 1
;;   :after company
;;   :config
;;   (eval-after-load 'company-tabnine
;;     (if (not (file-directory-p "~/.TabNine/"))
;; 	(company-tabnine-install-binary)))
;;   (add-to-list 'company-backends #'company-tabnine)
;;   )

(use-package company-org-block
  :defer t
  :custom
  (company-org-block-edit-style 'auto) ;; 'auto, 'prompt, or 'inline
  :hook ((org-mode . (lambda ()
		       (setq-local company-backends '(company-org-block))
		       (company-mode 1)))))

;;A fuzzy matching of company
(use-package company-flx
  :hook
  (company-mode . company-flx-mode))

;;simple and fast sorting and filtering framework for comppany
(use-package company-prescient
  :hook
  (company-mode . company-prescient-mode)
  :config
  (setq prescient-filter-method '(literal regexp initialism)))

;; (use-package company-quickhelp
;;   :hook
;;   (company-mode . company-quickhelp-mode)
;;   :config
;;   (setq x-gtk-use-system-tooltips nil)
;;   (setq company-quickhelp-tooltip 'posframe))

;;Alternative to company-quickhelp
(use-package company-posframe
  :hook
  (company-mode . company-posframe-mode)
  :custom
  (company-posframe-quickhelp-delay 0.3))

;; (use-package company-box
;;   :hook
;;   (company-mode . company-box-mode)
;;   :custom
;;   (company-box-enable-icon t)
;;   (company-box-icons-alist 'company-box-icons-all-the-icons))

;;To specify new version of git on remote machine so I can run magit locally
;;add ~/.ssh/config and ~/.ssh/known_hosts first
;;then ssh-keygen -t rsa => ssh-copy-id name@host_name
(use-package tramp
  :defer t
  :straight (:type built-in)
  :if (memq system-type '(gnu/linux darwin))
  :config
  ;; use for debug
  ;; (setq tramp-verbose 6)
  (add-to-list 'tramp-remote-path "/usr/local/bin/git")
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  ;;tramp mode to cache password
  (setq password-cache-expiry nil))

(use-package counsel-tramp
  :after (counsel tramp)
  :config
  (setq counsel-tramp-custom-connections '("/ssh:mogu@almond.ggy.bris.ac.uk:/home/mogu/cgenie.muffin/"
					   "/ssh:mogu@sprout.ggy.bris.ac.uk:/home/mogu/cgenie.muffin/"))
  (setq tramp-default-method "ssh")
  (setq make-backup-files nil)
  (setq create-lockfiles nil)
  :bind
  ("C-c s" . counsel-tramp))

;; visit https://github.com/jacktasia/dumb-jump to see more alternative ways
;; like TAGS system
;;======================================================================
;;depends on external programs: The-Silver-Searcher/ripgrep and emacs package ag/rg
;;======================================================================
(use-package dumb-jump
  :defer 4
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-default-project "~/cgenie.muffin")
  (add-to-list 'auto-mode-alist '("\\.config\\'" . shell-script-mode)))

(use-package ag :defer t)
(use-package rg :defer t)
(use-package ripgrep :defer t)

(use-package which-key
  :hook
  (after-init . which-key-mode))

;; on-the-fly syntax checker,  use C-c ! as prefix, e.g., C-c ! v to verify the checkers
;; use M-g n/p to navigate error, or use C-c e (counsel-flycheck)

;; python flycheck depdencies
;; pip install pyflakes -> fast and don't check code style
(use-package flycheck-pyflakes)

;; Alternative
;; pip install prospector -> don't check code style but relatively slower
;; (use-package flycheck-prospector
;;   :config
;;   (flycheck-prospector-setup)
;;   )

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

(use-package yasnippet
  :straight yasnippet-snippets ;; Collection of snippets
  :hook (after-init . yas-global-mode))

;;manually choose a snippet
(use-package ivy-yasnippet
  :after (ivy yasnippet)
  :bind
  (("C-c i" . ivy-yasnippet))
  :config
  (setq ivy-yasnippet-expand-keys 'smart))

;;Git + Emacs = boom!
(use-package magit
  :bind
  ("C-x g" . magit-status)
  ("C-x c" . magit-checkout))

;;a magit prefix help page
(use-package transient
  :defer t)

;;This package reads proper environment variable in MacOS GUI version
;;To speed up this package, (1) separate configuration into
;;non-interactive (.zshenv) and interactive (.zshrc) part;
;;(2) set explicit path in .zshenv (which is what we will use, you should
;;put your PATH variable like /usr/local/bin/python3.9 in this file)
;;Find out more in https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (setq exec-path-from-shell-arguments nil) ;;read non-interactive shell config
  (exec-path-from-shell-initialize)
  )

(use-package use-package-ensure-system-package
  :defer t
  :after exec-path-from-shell) ;;extend use-package, put after exec-path-from-shell

(use-package popwin
  :hook
  (after-init . popwin-mode))

(use-package ivy
  :hook
  (after-init . ivy-mode)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-wrap t)
  (setq ivy-height 9)
  ;;  (setq ivy-format-function 'ivy-format-function-line)
  :bind
  (("\C-s" . swiper)
   ("C-c v" . ivy-push-view)
   ("C-c V" . ivy-pop-view)
   :map ivy-minibuffer-map
   ("TAB" . ivy-alt-done)
   :map ivy-switch-buffer-map
   ("C-d" . ivy-switch-buffer-kill)))

(use-package counsel
  :after ivy
  :bind
  (;; ("C-x C-b" . counsel-ibuffer)
   ("C-x b" . counsel-switch-buffer)
   ("C-c b" . counsel-imenu) ;; imenus provides a list of definition
   ("C-x C-f" . counsel-find-file)
   ("M-x" . counsel-M-x)
   ("M-y" . counsel-yank-pop) ;;something like a clipboard
   ("C-h f" . counsel-describe-function)
   ("C-h v" . counsel-describe-variable)
   ("C-c t" . counsel-load-theme)
   ("C-c j" . counsel-git-grep)
   ("C-c g" . counsel-git) ;;find file in current git directory
   ("C-c l" . counsel-git-log)
   ("C-c r" . counsel-rg)  ;;rg find tex
   ("C-c f" . counsel-fzf) ;;fzf find file
   ("C-c e" . counsel-flycheck)
   ("C-c C-r" . counsel-recentf)
   :map minibuffer-local-map
   ("C-r" . counsel-minibuffer-history)
   ))

;;sorting and filtering framework for ivy
(use-package ivy-prescient
  :hook
  (ivy-mode . ivy-prescient-mode)
  :config
  (setq ivy-prescient-sort-commands t
	ivy-prescient-enable-sorting nil
	ivy-prescient-retain-classic-highlighting t))

;; Project management tool
(use-package projectile
  :after ivy
  :config
  (setq projectile-completion-system 'ivy)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package counsel-projectile
  :hook
  (after-init . counsel-projectile-mode))

;; yet another robust find file in project
;; but don't rely on fzf
(use-package find-file-in-project
  :bind
  ("C-x f" . find-file-in-project-at-point))

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

;; yet another search system
;; (use-package consult
;;   ;; Replace bindings. Lazily loaded due by `use-package'.
;;   :bind (;; C-c bindings (mode-specific-map)
;;          ("C-c h" . consult-history)
;;          ("C-c m" . consult-mode-command)
;;          ("C-c b" . consult-bookmark)
;;          ("C-c k" . consult-kmacro)
;;          ;; C-x bindings (ctl-x-map)
;;          ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
;;          ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
;;          ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
;;          ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
;;          ;; Custom M-# bindings for fast register access
;;          ("M-#" . consult-register-load)
;;          ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
;;          ("C-M-#" . consult-register)
;;          ;; Other custom bindings
;;          ("M-y" . consult-yank-pop)                ;; orig. yank-pop
;;          ("<help> a" . consult-apropos)            ;; orig. apropos-command
;;          ;; M-g bindings (goto-map)
;;          ("M-g e" . consult-compile-error)
;;          ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
;;          ("M-g g" . consult-goto-line)             ;; orig. goto-line
;;          ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
;;          ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
;;          ("M-g m" . consult-mark)
;;          ("M-g k" . consult-global-mark)
;;          ("M-g i" . consult-imenu)
;;          ("M-g I" . consult-project-imenu)
;;          ;; M-s bindings (search-map)
;;          ("M-s f" . consult-find)
;;          ("M-s L" . consult-locate)
;;          ("M-s g" . consult-grep)
;;          ("M-s G" . consult-git-grep)
;;          ("M-s r" . consult-ripgrep)
;;          ("M-s l" . consult-line)
;;          ("M-s m" . consult-multi-occur)
;;          ("M-s k" . consult-keep-lines)
;;          ("M-s u" . consult-focus-lines)
;;          ;; Isearch integration
;;          ("M-s e" . consult-isearch)
;;          :map isearch-mode-map
;;          ("M-e" . consult-isearch)                 ;; orig. isearch-edit-string
;;          ("M-s e" . consult-isearch)               ;; orig. isearch-edit-string
;;          ("M-s l" . consult-line))                 ;; needed by consult-line to detect isearch

;;   ;; Enable automatic preview at point in the *Completions* buffer.
;;   ;; This is relevant when you use the default completion UI,
;;   ;; and not necessary for Vertico, Selectrum, etc.
;;   :hook (completion-list-mode . consult-preview-at-point-mode)

;;   ;; The :init configuration is always executed (Not lazy)
;;   :init

;;   ;; Optionally configure the register formatting. This improves the register
;;   ;; preview for `consult-register', `consult-register-load',
;;   ;; `consult-register-store' and the Emacs built-ins.
;;   (setq register-preview-delay 0
;;         register-preview-function #'consult-register-format)

;;   ;; Optionally tweak the register preview window.
;;   ;; This adds thin lines, sorting and hides the mode line of the window.
;;   (advice-add #'register-preview :override #'consult-register-window)

;;   ;; Optionally replace `completing-read-multiple' with an enhanced version.
;;   (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

;;   ;; Use Consult to select xref locations with preview
;;   (setq xref-show-xrefs-function #'consult-xref
;;         xref-show-definitions-function #'consult-xref)

;;   ;; Configure other variables and modes in the :config section,
;;   ;; after lazily loading the package.
;;   :config

;;   ;; Optionally configure preview. The default value
;;   ;; is 'any, such that any key triggers the preview.
;;   ;; (setq consult-preview-key 'any)
;;   ;; (setq consult-preview-key (kbd "M-."))
;;   ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
;;   ;; For some commands and buffer sources it is useful to configure the
;;   ;; :preview-key on a per-command basis using the `consult-customize' macro.
;;   (consult-customize
;;    consult-theme
;;    :preview-key '(:debounce 0.2 any)
;;    consult-ripgrep consult-git-grep consult-grep
;;    consult-bookmark consult-recent-file consult-xref
;;    consult--source-file consult--source-project-file consult--source-bookmark
;;    :preview-key (kbd "M-."))

;;   ;; Optionally configure the narrowing key.
;;   ;; Both < and C-+ work reasonably well.
;;   (setq consult-narrow-key "<") ;; (kbd "C-+")

;;   ;; Optionally make narrowing help available in the minibuffer.
;;   ;; You may want to use `embark-prefix-help-command' or which-key instead.
;;   ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)


;;   ;;;; 2. projectile.el (projectile-project-root)
;;   (autoload 'projectile-project-root "projectile")
;;   (setq consult-project-root-function #'projectile-project-root)
;;   ;;;; 3. vc.el (vc-root-dir)
;;   ;; (setq consult-project-root-function #'vc-root-dir)
;;   ;;;; 4. locate-dominating-file
;;   ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))
;; )

;; Another interesting package for fuzzy finding
;; (use-package affe
;;   :config
;;   ;; Manual preview key for `affe-grep'
;;   (consult-customize affe-grep :preview-key (kbd "M-.")))

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

;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Mark set
;; (global-set-key (kbd "C-j") 'set-mark-command)
;; C-x C-x -> set mark and move back to previous position
;; C-x h to select all

;; a human-friendly keymap of built-in code-folding package
;; alternatives: vimish-fold, Origami
(add-hook 'prog-mode-hook #'hs-minor-mode)
(global-set-key (kbd "<f5>") 'hs-toggle-hiding)

;; adjust font size
(setq-default text-scale-mode-step 1.1)

;; one "BODY", many "HEADs", try "C-c 5 =" to know the magic
(use-package hydra
  :config
  (defhydra hydra-zoom (global-map "C-c")
    "zoom in/out windows, or increase/decrease text"
    ("=" text-scale-increase "in")
    ("-" text-scale-decrease "out"))
  (defhydra hydra-vi (:pre (set-cursor-color "#40e0d0")
			   :post (progn
				   (set-cursor-color "#ffffff")
				   (message
				    "Thank you, come again.")))
    "vi-style movement in Emacs"
    ("l" forward-char)
    ("h" backward-char)
    ("j" next-line)
    ("k" previous-line)
    ("q" nil "quit"))
  (global-set-key (kbd "C-c v") 'hydra-vi/body))

;; a center-floating posframe for hydra
(use-package hydra-posframe
  :straight
  (:type git :host github
	 :repo "Ladicle/hydra-posframe")
  :hook
  (after-init . hydra-posframe-mode))

;; define a hydra-leader-key for major mode, something like C-c
(use-package major-mode-hydra
  :after all-the-icons
  :bind
  ("S-SPC" . major-mode-hydra)
  :config
  (defvar hydra-r-title (s-concat
			 (s-repeat 28 " ")
			 (all-the-icons-icon-for-file "f.R")
			 " "
			 "ESS R commands"))
  (defvar hydra-py-title (s-concat
			  (s-repeat 23 " ")
			  (all-the-icons-icon-for-file "f.py")
			  " "
			  "Python mode commands"))
  (defvar hydra-el-title (s-concat
			  (s-repeat 18 " ")
			  (all-the-icons-icon-for-file "f.el")
			  " "
			  "Emacs-lisp commands"))
  (defvar hydra-tex-title (s-concat
			   (s-repeat 23 " ")
			   (all-the-icons-icon-for-file "f.tex")
			   " "
			   "LaTeX commands"))
  (major-mode-hydra-define emacs-lisp-mode
    (:foreign-keys warn :quit-key "q"
		   :title hydra-el-title :separator "-")
    ("Eval"
     (("b" eval-buffer "buffer")
      ("e" eval-defun "defun")
      ("r" eval-region "region"))
     "REPL"
     (("I" ielm "ielm"))
     "Doc"
     (("d" describe-foo-at-point "thing-at-pt")
      ("f" counsel-describe-function "function")
      ("v" counsel-describe-variable "variable")
      ("i" info-lookup-symbol "info lookup"))
     "Project"
     (("p D" projectile-dired "dired")
      ("p f" projectile-find-file "find")
      ("p d" projectile-find-directory "directory")
      ("p s" projectile-ripgrep "search")
      ("p p" projectile-switch-project "switch")
      ("p r" projectile-replace "replace"))))
  (major-mode-hydra-define python-mode
    (:foreign-keys warn :quit-key "q"
		   :title hydra-py-title :separator "-")
    ("Execute"
     (("b" python-shell-send-buffer "buffer")
      ("r" python-shell-send-region "region")
      ("f" python-shell-send-file "file")
      ("j" ein:run "jupyter"))
     "REPL"
     (("c" run-python "console")
      ("s" shell "shell")
      ("u" undo-tree-mode "undo-tree")
      ("y" ivy-yasnippet "yasnippet")
      ("g" magit-status "git"))
     "Find"
     (("i" counsel-imenu "function")
      ("r" counsel-rg "ripgrep")
      ("F" counsel-fzf "find"))
     "Check"
     (("l" flycheck-list-errors "all")
      ("c" flycheck-clear "clear")
      ("v" flycheck-verify-setup "setup"))
     "Project"
     (("p D" projectile-dired "dired")
      ("p f" projectile-find-file "find")
      ("p d" projectile-find-directory "directory")
      ("p s" projectile-ripgrep "search")
      ("p p" projectile-switch-project "switch")
      ("p r" projectile-replace "replace"))))
  (major-mode-hydra-define ess-r-mode
    (:foreign-keys warn :quit-key "q"
		   :separator "-"
		   :title hydra-r-title)
    ("Run"
     (("b" ess-eval-buffer-and-go "buffer")
      ("r" ess-eval-region-and-go "region")
      ("f" ess-eval-function-and-go "function"))
     "Power"
     (("c" run-ess-r "console")
      ("s" shell "terminal")
      ("u" undo-tree-mode "undo-tree")
      ("y" ivy-yasnippet "yasnippet")
      ("g" magit-status "git"))
     "Find"
     (("i" counsel-imenu "function")
      ("R" counsel-rg "ripgrep")
      ("F" counsel-fzf "find"))
     "Check"
     (("l" flycheck-list-errors "all")
      ("c" flycheck-clear "clear")
      ("v" flycheck-verify-setup "setup"))
     "Project"
     (("p D" projectile-dired "dired")
      ("p f" projectile-find-file "find")
      ("p d" projectile-find-directory "directory")
      ("p s" projectile-ripgrep "search")
      ("p p" projectile-switch-project "switch")
      ("p r" projectile-replace "replace"))))
  (major-mode-hydra-define latex-mode
    (:foreign-keys warn :quit-key "q"
		   :separator "-"
		   :title hydra-tex-title)
    ("Compile"
     (("m" TeX-command-master "master-cmd")
      ("v" TeX-view "view"))
     "Preview"
     (("p p" math-preview-at-point "preview-at-pt")
      ("p a" math-preview-all "preview-all")
      ("p a" math-preview-region "preview-region")
      ("c p" math-preview-clear-at-point "clear-at-pt")
      ("c a" math-preview-clear-all "clear-all")
      ("c r" math-preview-clear-region "clear-region"))
     "Insert"
     (("e" LaTeX-environment "env")
      ("]" LaTeX-close-environment "close env")
      ("f" TeX-font "font")
      ("s" LaTeX-section "section")
      ("i" LaTeX-insert-item "item"))
     "Languagetool"
     (("c" languagetool-check "check")
      ("d" languagetool-clear-buffer "clear")
      ("i" languagetool-correct-at-point "at-pt")
      ("b" languagetool-buffer "buffer")
      ("S" languagetool-set-language "setting")))))

;; beautify hydra
(use-package pretty-hydra
  :after all-the-icons
  :config
  (defvar hydra-ui-title (s-concat (s-repeat 20 " ")
				   (all-the-icons-faicon "windows")
				   " Apperance"))
  (pretty-hydra-define hydra-ui
    (:foreign-keys warn :title hydra-ui-title :quit-key "q")
    ("Theme"
     (("d" night-theme "dark-theme")
      ("l" day-theme "light-theme")
      ("t" counsel-load-theme "choose"))
     "Window"
     (("b" split-window-right "split horizontally")
      ("v" split-window-below "split vertically")
      ("f" toggle-frame-fullscreen "fullscreen")
      ("m" ace-delete-other-windows "maximize")
      ("o" ace-window "others"))
     "Page"
     (("n" forward-page "next")
      ("p" backward-page "previous"))))
  (global-set-key (kbd "C-c w") 'hydra-ui/body))

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
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)) ;;same title bar color
    (add-to-list 'default-frame-alist '(ns-appearance . light))))

;;Auto-max the frame at startup
(defun auto-max-frame ()
  "Maxize/full screen the frame according to the OS type."
  (interactive)
  (if (eq system-type 'darwin)
      (toggle-frame-maximized)
    (toggle-frame-fullscreen)))

(if (display-graphic-p)
    (add-hook 'after-init-hook #'auto-max-frame))

;;no more startup message/screen
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)

;;Display time in the mode line
(add-hook 'after-init-hook 'display-time-mode)
(setq display-time-format "%B %d %H:%M %p")
(setq system-time-locale nil)

;;If use doom-mode-line, then display battery
(add-hook 'doom-modeline-mode-hook 'display-battery-mode)

;;Don't display load average percentage
(setq display-time-default-load-average nil)

;;Cursor
(setq-default cursor-type 'bar)
(blink-cursor-mode 0)

;;Highlight current line
(add-hook 'after-init-hook 'global-hl-line-mode)

;; (use-package yascroll
;;   :hook
;;   (after-init . global-yascroll-bar-mode))

(use-package ivy-posframe
  :config
  (setq ivy-posframe-display-functions-alist
	'((complete-symbol . ivy-posframe-display-at-point)
	  (swiper . ivy-posframe-display-at-window-center)
	  (counsel-M-x . ivy-posframe-display-at-window-center)
	  (t . ivy-posframe-display)))
  (setq ivy-posframe-parameters
	'((left-fringe . 8)
	  (right-fringe . 8)))
  (setq ivy-posframe-width 200
	ivy-posframe-border-width 0)
  (setq ivy-posframe-height-alist '((swiper . 10)
				    (t . 10)))
  ;; fix the width
  (defun ivy-posframe-get-size ()
    "Set the ivy-posframe size according to the current frame."
    (let ((height (or ivy-posframe-height (or ivy-height 10)))
	  (width (min (or ivy-posframe-width 200) (round (* 0.75 (frame-width))))))
      (list :height height :width width :min-height height :min-width width)))
  (setq ivy-posframe-size-function 'ivy-posframe-get-size)
  :hook
  (ivy-mode . ivy-posframe-mode))

;; similar to ivy-frame
;; (use-package mini-frame
;;   :hook
;;   (after-init . mini-frame-mode)
;;   :config
;;   (setq resize-mini-frames t)
;;   ;; for gnome shell
;;   ;; (setq x-gtk-resize-child-frames 'resize-mode)
;;   :custom
;;   (mini-frame-show-parameters
;;    '((top . 0.25)
;;      (width . 0.7)
;;      (left . 0.5)))
;;   (mini-frame-ignore-commands
;;    '(eval-expression "edebug-eval-expression"
;; 		     debugger-eval-expression swiper))
;;   (mini-frame-create-lazy nil))

;; highlight cursor when scroll window
(use-package beacon
  :straight (:type git :host github
		   :repo "Malabarba/beacon")
  :hook
  (after-init . beacon-mode))

;; type-writer sound effect
;; (use-package selectric-mode
;;   :hook
;;   (after-init . selectric-mode))

;; highlight a little bit "important" buffers
;; depends on what theme you're using
(use-package solaire-mode
  :hook
  (after-init . solaire-global-mode))

;; minimal columns for Emacs to split window horizontally
(setq split-width-threshold 130)

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

;;turn off electric-indent-mode but use aggressive-indent-mode
(electric-indent-mode 1)

;; (use-package aggressive-indent
;;   :hook
;;   (prog-mode . aggressive-indent-mode))

(setq frame-inhibit-implied-resize nil)

;;-----------Dired enhancement-------------
;; (use-package dired-hacks-utils
;;   :hook
;;   (dired-mode . dired-utils-format-information-line-mode)
;;   )

(setq dired-listing-switches "-alFh")
(with-eval-after-load 'dired
  (define-key dired-mode-map [mouse-2] 'dired-mouse-find-file))

;; W -> X to move, W -> Y to copy
(use-package dired-ranger
  :bind
  (:map dired-mode-map
	("W" . dired-ranger-copy)
	("X" . dired-ranger-move)
	("Y" . dired-ranger-paste)))

;; (use-package dired-collapse
;;   :hook
;;   (dired-mode . dired-collapse-mode))

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
(setq electric-pair-pairs '(
			    (?\" . ?\")
			    (?\` . ?\`)
			    (?\( . ?\))
			    (?\{ . ?\})))

;; use lispy-mode (a vi-like editing) for lisp parentheses
;; remove electric-pair-mode first
(add-hook 'emacs-lisp-mode-hook (lambda ()
				  (electric-pair-local-mode -1)))

;; >>> Basic lispy usage:
;; jkhl to move, f/b to foward/backward level
;; c to copy, m to mark, e to evaluate, d to swith parenthesis side (C-d to delete)
;; >/< to slurp/barf: push out/pull in
;; w/s to move marked regions up/down
;; M-j to split, + to join
;; To insert a single parenthsis, use a C-q prefix

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

;; --> Option 2 (Advanced but has learning curve)
;; (use-package paredit
;;   :config
;;   (paredit-mode t)
;;   )

;; (use-package paredit-everywhere
;;   :config
;;   (add-hook 'prog-mode-hook 'paredit-everywhere-mode)
;;   )

;; --> Option 3 (ISSUE: can't close several brackets by one pressing)
;; more see the doc https://github.com/Fuco1/smartparens/wiki/Working-with-expressions
;; https://github.com/lujun9972/emacs-document/blob/master/emacs-common/Smartparens用法详解.org
;; (use-package smartparens
;; :hook
;;  (after-init . smartparens-global-mode)
;; :bind
;; (:map smartparens-mode-map
;; 	("C-M-f" . sp-forward-sexp)
;; 	("C-M-b" . sp-backward-sexp)
;; 	("C-M-h" . sp-down-sexp) ;;down one level
;; 	("C-M-l" . sp-up-sexp) ;;up one level
;; 	("M-[" . sp-backward-unwrap-sexp)
;; 	("C-M-k" . sp-kill-sexp))
;; :config
;; (sp-pair "\{" "\}") ;; latex literal brackets (included by default)
;; (sp-pair "<#" "#>")
;; (sp-pair "$" "$")   ;; latex inline math mode. Pairs can have same opening and closing string)
;; (sp-local-pair 'LaTeX-mode "\\left(" "\\right)" :insert "C-b l" :trigger "\\l(")
;; (sp-pair "'" nil :actions :rem))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package minions
  :config
  (minions-mode 1)
  :bind
  ([S-down-mouse-3] . minions-minor-modes-menu))

;; (use-package spaceline
;;  :config
;;  (require 'spaceline-config)
;;  :config
;;  (spaceline-emacs-theme))

(use-package mood-line
  :hook
  (after-init . mood-line-mode))

;; yet another simple and clean mode line
;; (use-package awesome-tray
;;   :straight (:type git :host github
;; 		   :repo "manateelazycat/awesome-tray")
;;   :hook
;;   (after-init . awesome-tray-mode))

;; a feature-rich modeline
;; (use-package doom-modeline
;;   ;;right fringe cut-off issue should relate to font size
;;   ;;Use cnfont-decrease-size or see more methods in
;;   ;;https://github.com/hlissner/doom-emacs/blob/develop/modules/ui/modeline/README.org#the-right-side-of-the-modeline-is-cut-off
;;   :after all-the-icons
;;   :hook (after-init . doom-modeline-mode)
;;   :config
;;   (setq doom-modeline-window-width-limit fill-column)
;;   (setq doom-modeline-icon (display-graphic-p))
;;   (setq doom-modeline-major-mode-icon t)
;;   (setq doom-modeline-enable-word-count nil)
;;   (setq all-the-icons-scale-factor 1.0)
;;   )

;; show emoji
(use-package emojify
  :hook
  (after-init . global-emojify-mode)
  :custom
  (emojify-display-style 'unicode)
  (emojify-download-emojis-p t)
  (emojify-emoji-styles '(ascii github unicode)))

(use-package nyan-mode
  :hook
  (doom-modeline-mode . nyan-mode))

;; defer if it's slow
(use-package dashboard
  :if (and (< (length command-line-args) 2)
	   (fboundp 'native-comp-available-p))
  :config
  (setq dashboard-set-init-info nil)
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Happiness is everything - Rui")
  ;;    (setq dashboard-startup-banner 3)
  (setq dashboard-startup-banner "~/.emacs.d/fancy-splash/world.png")
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents . 3))) ;;add org-agenda could slow start-up speed
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))) ;; show Dashboard in frames created with emacsclient -c
  (setq dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)
  (define-key dashboard-mode-map (kbd "n") 'next-line)
  (define-key dashboard-mode-map (kbd "p") 'previous-line))

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

(use-package minimap
  :custom
  (minimap-window-location 'right)
  (minimap-width-fraction 0.05)
  (minimap-minimum-width 15)
  :bind
  ("<f6>" . minimap-mode))

(use-package all-the-icons)

;;(use-package neotree
;;  :config
;;  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
;;  (global-set-key [f8] 'neotree-toggle)
;;  )

(use-package treemacs
  :bind
  ("<f8>" . treemacs))

(use-package treemacs-all-the-icons
  :defer t
  :requires
  (treemacs all-the-icons)
  :config
  (treemacs-load-theme "all-the-icons"))

(use-package all-the-icons-dired
  :if window-system
  ;;need to run all-the-icons-install-fonts first to avoid grabled icon
  :hook
  (dired-mode . all-the-icons-dired-mode))

(use-package ivy-rich
  :hook
  (ivy-mode . ivy-rich-mode)
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (setq ivy-rich-path-style 'absolute)
  :custom
  (ivy-rich-modify-columns
   'ivy-switch-buffer
   '((ivy-rich-switch-buffer-size (:align right)))
   'counsel-M-x
   '((counsel-M-x-transformer (:width 40)))
   ))

(use-package all-the-icons-ivy-rich
  :if window-system
  :after ivy-rich
  :config
  (setq all-the-icons-ivy-rich-icon-size 1.0)
  (setq inhibit-compacting-font-caches t)
  (all-the-icons-ivy-rich-mode t))

;; Enable icons in the ibuffer
(use-package all-the-icons-ibuffer
  :if window-system
  :config
  (setq inhibit-compacting-font-caches t)
  :hook
  (ibuffer-mode . all-the-icons-ibuffer-mode))

(use-package centaur-tabs
  :config
  ;;  (centaur-tabs-mode t)
  (setq centaur-tabs-set-bar 'over)
  (setq centaur-tabs-gray-out-icons 'buffer)
  ;;(setq centaur-tabs-set-icons t)
  (setq centaur-tabs-plain-icons t)
  (setq centaur-tabs-close-button "x")
  (setq centaur-tabs-set-modified-marker t)
  (setq centaur-tabs-modified-marker "*")
  (setq centaur-tabs-height 20)
  ;;(setq centaur-tabs-label-fixed-length 10) ;;fixed length
  ;; (centaur-tabs-change-fonts "Roboto Mono" 130)
  (setq centaur-tabs-show-navigation-buttons nil)
  :bind
  ("M-<left>" . centaur-tabs-backward)
  ("M-<right>" . centaur-tabs-forward)
  :hook
  (dired-mode . centaur-tabs-local-mode)
  (after-init . centaur-tabs-mode))

;;press your keyboard fast and hard !!!
(use-package power-mode
  :defer t
  :straight (power-mode :type git :host github
			:repo "elizagamedev/power-mode.el"))

;; to display ^L page break
(use-package form-feed
  :hook
  (emacs-lisp-mode . form-feed-mode))

;; (use-package focus
;;   :defer t
;;   :hook
;;   (text-mode . focus-mode)
;;   :config
;;   (add-to-list 'focus-mode-to-thing '((text-mode . sentence)
;; 				      (prog-mode . defun)
;; 				      (latex-mode . paragraph))))

;; (use-package smooth-scroll
;;   :load-path "extra-lisp/"
;;   :hook
;;   (after-init . smooth-scroll-mode)
;;   )


;;; FONT, THEME & COLOR SCHEME

;;English font: Iosevka/Inconsolata/Juliamono/Jetbrains Mono/Roboto Mono/Monaco/Fira Code/SF Mono/Operator Mono
;;Chinese font: Wenquanyi Micro Hei Mono/Sarasa UI SC Mono/Sarasa Mono SC Nerd/Noto Sans CJK SC Mono (work perfectly with Iosevka/Inconsolata)
;;Variable-pitch font, ETBembo/New York
;;Unicode: Symbola

(defun init-font ()
  "Set English and CJK font for Emacs."
  (interactive)
  ;; English font
  (if (display-graphic-p)
      (progn
	;; English font
	(set-face-attribute 'default nil :font (format "%s:pixelsize=%d" "SF Mono" 16))
	;; CJK font
	(dolist (charset '(kana han symbol cjk-misc bopomofo))
	  (set-fontset-font (frame-parameter nil 'font)
			    charset
			    (font-spec :family "Sarasa Mono SC Nerd"))))))

;; Use emacs daemon, put following lines to shell config file
;; alias ed="emacs --daemon"
;; alias ec="emacsclient -c"
;; alias eq="emacsclient -e '(save-buffers-kill-emacs)'"

;; Set font and auto-fullscreen in daemon-mode, put after init-ui.el
(if (daemonp)
    (add-hook 'after-make-frame-functions
	      (lambda (frame)
		(with-selected-frame frame
		  (init-font)
		  (auto-max-frame))))
  (add-hook 'after-init-hook 'init-font))

;;Install themes
(use-package base16-theme :defer t )
(use-package color-theme-sanityinc-tomorrow :defer t )
(use-package gruvbox-theme :defer t )
(use-package tao-theme :defer t )
(use-package humanoid-themes :defer t )
(use-package twilight-bright-theme :defer t )
(use-package ample-theme :defer t )
(use-package eziam-theme :defer t ) ;;almost perfect light theme
(use-package spacemacs-common :defer t :straight spacemacs-theme)
(use-package doom-themes
  :defer t
  :config
  ;;treemacs setting
  (setq doom-themes-treemacs-enable-variable-pitch nil)
  (setq doom-themes-treemacs-theme "doom-color")
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; loading theme
(setq custom-safe-themes t)
(setq-default custom-enabled-themes '(doom-city-lights))

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes ',custom-enabled-themes)))

(add-hook 'after-init-hook 'reapply-themes)

;; Toggle between light and dark
(defun day-theme ()
  "Activate a light color theme.  Recommendation: leuven, spacemacs-light,
   eziam, twilight-bright, modus-operandi."
  (interactive)
  (setq custom-enabled-themes '(doom-nord-light))
  (reapply-themes))

(defun night-theme ()
  "Activate a dark color theme.  Recommendation: humanoid-dark, doom-city-light,
  doom-one/vibrant, doom-dark+, sanityinc-tomorrow-night, doom-wilmersdorf"
  (interactive)
  (setq custom-enabled-themes '(doom-vibrant))
  (reapply-themes))

;;Transprancy setting
(set-frame-parameter (selected-frame) 'alpha '(97 100))
(add-to-list 'default-frame-alist '(alpha 97 100))

;;Font Setting
(setq inhibit-compacting-font-caches t)

;;Varialble/fixed pictch font setting, essential for org-mode
(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "Noto Serif" :height 160))))
 '(fixed-pitch ((t ( :family "Roboto Mono" :height 160)))))

(custom-theme-set-faces
 'user
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-link ((t (:foreground "light blue" :underline t))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8)))))

(custom-set-faces
 '(org-level-1 ((t (:inherit fixed-pitch :height 1.2))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.1))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.1))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))

;;Unicode font setting
(when (member "Symbola" (font-family-list))
  (set-fontset-font "fontset-default" nil
		    (font-spec :size 16 :name "Symbola")))

(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))


;;; PROGRAMMING LANGUAGES & LSP

;;==============================
;;            Eglot           ;;
;;==============================
;;eglot can work with tramp-mode, but you should install
;;your server-programs on remote, not local
(use-package eglot
  :config
  (add-hook 'eglot-managed-mode-hook (lambda () (flymake-mode -1))) ;;Decouple flymake and eglot
  ;;============================================
  ;; make sure every command works separately in shell environment
  (set 'ad-redefinition-action 'accept)
  ;; install clangd first
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) ("clangd")))
  ;; pip3 install fortran-language-server
  (add-to-list 'eglot-server-programs '(f90-mode . ("fortls")))
  ;; use tex-lab or digestif as TeX server
  (add-to-list 'eglot-server-programs '((LaTeX-mode tex-mode context-mode texinfo-mode bibtex-mode)
					. ("texlab")))
  ;; pip3 install python-lsp-server
  ;; jupterlab has some experimental lsp server, install and change it above: pip3 install git+https://github.com/krassowski/python-language-server.git@main
  (add-to-list 'eglot-server-programs '(python-mode . ("pylsp")))
  ;; install.packages("languageserver")
  ;; Note R can be tricky in zsh due to the built-in command "r"
  ;; more R lsp server setting can be done in .Rprofile or .lintr file
  (add-to-list 'eglot-server-programs '(ess-r-mode . ("R" "--slave" "-e" "languageserver::run()")))
  ;;============================================
  :hook
  (python-mode . eglot-ensure)
  (f90-mode . eglot-ensure)
  (ess-r-mode . eglot-ensure)
  (LaTeX-mode . eglot-ensure)
  ;;============================================
  ;;local keybindings
  :bind
  (:map eglot-mode-map
	("C-c r" . eglot-rename)
	("C-c h" . eldoc))
  ;;or add follwing lines to :config section
  ;;(define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
  ;;(define-key eglot-mode-map (kbd "C-c h") 'eldoc)
  )

;;==============================
;;           Python           ;;
;;==============================
;;(setq python-shell-interpreter "python3.9")
;; or ipython
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True")

;;python-style indent
(setq python-indent-offset 4)
(setq python-indent-guess-indent-offset-verbose nil) ;;don't complain about the indent anymore

;;debug setting
(setq python-shell-completion-native-enable nil) ;;or pip3 install pyreadline to avoid warning
(setq python-shell-prompt-detect-failure-warning nil)
(setq python-shell-enable-font-lock nil) ;;make printing fast
(setq python-shell-completion-native-enable nil) ;;don't use ipython completion, cause bug

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

(use-package conda
  :config
  (conda-env-initialize-interactive-shells)
  (if (file-directory-p "~/miniconda3")
      (setq conda-anaconda-home (expand-file-name "~/miniconda3")
	    conda-env-home-directory (expand-file-name "~/miniconda3"))
    (setq conda-anaconda-home (expand-file-name "~/anaconda")
	  conda-env-home-directory (expand-file-name "~/anaconda")))
  ;; when in conda-project-env-name or has environmental.yml auto activate
  (conda-env-autoactivate-mode t)
  :bind
  ("C-c c a" . conda-env-activate)
  ("C-c c d" . conda-env-deactivate))

;;jupyter notebook integration

;;>>> option 1
;; (use-package jupyter
;;  :defer t)
;;1. Require Emacs with module-support
;;2. run `pip install ipykernel` `python -m ipykernel install --user`
;;3. This package use zmq which makes Emacs very slow

;;>>> option 2
;; specify jupyter kernel in kernel.json file
;; if you don't know its path, run !jupyter kernelspec list in ipython
;; Other checking commands:
;; import sys; print(sys.executable); print(sys.path)
;; I also recommend to use mamba to manage packages
;; ---org-babel snippet---
;;#+BEGIN_SRC ein-python :session localhost:8889
;;#+END_SRC
;; Change cell type by C-c C-t

(use-package ein
  :defer 1
  :config
  ;;ein-babel config see the org-mode block
  (setq ein:use-company-backend t)
  (setq ein:worksheet-enable-undo t)
  (setq ein:output-area-inlined-images t)
  (add-hook 'poly-ein-mode-hook 'elpy-enable)
  (add-hook 'poly-ein-mode-hook (lambda ()
				  (display-line-numbers-mode nil))) ;;avoid grabled line-number
  (with-eval-after-load 'ein-notebook
    (define-key ein:notebook-mode-map "\C-c\C-d" 'ein:worksheet-delete-cell))
  (setq ein:worksheet-enable-undo t))

(use-package elpy :defer t) 		;;a completion system for ein

;;==============================
;;           Rlang            ;;
;;==============================
;; require ESS installed
;;Lazy load ess-r-mode (ESS doesn't like use-package pretty much)
(use-package ess :defer t)

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
  :after ess
  :hook
  (ess-r-mode . rainbow-mode))

;;==============================
;;           Matlab           ;;
;;==============================
;; cd /path/to/matlab-emacs -> make
;; Homepage: https://sourceforge.net/p/matlab-emacs/src/ci/documentation/tree/
(use-package matlab-mode
  :defer t
  :mode "\\.[mM]\\'")


;;; Text-mode: Markdown/org-mode/TeX

;; ==============================
;;            Markdown         ;;
;; ==============================

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

;; >>> Option 1
;; depend on grip (pip install grip) and manual changing of github api limit
;; Not work for SSH git repository

;; (use-package grip-mode
;;
;;   :bind (:map markdown-mode-command-map
;;               ("g" . grip-mode))
;;   :hook (markdown-mode . grip-mode)
;;   :config
;;   ;;create your personal access token, then config
;;   ;;your github username and token in "~/.authinfo.gpg"
;;   ;;DO NOT input your password here!
;;   (require 'auth-source)
;;   (let ((credential (auth-source-user-and-password "api.github.com")))
;;     (setq grip-github-user (car credential)
;;           grip-github-password (cadr credential)))
;;   )

;; >>> Option 2
(use-package markdown-preview-mode
  :hook (markdown-mode . markdown-preview-mode)
  :bind
  (:map markdown-mode-map
	("C-c C-c p" . markdown-preview-open-browser))
  :config
  ;; extra css element
  (add-to-list 'markdown-preview-stylesheets "https://raw.githubusercontent.com/richleland/pygments-css/master/emacs.css")
  ;; enable mathjax
  (add-to-list 'markdown-preview-javascript "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML"))

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

;; ==============================
;;           Org-mode          ;;
;; ==============================
(use-package org
  :straight (:type built-in)
  :after counsel
  :config
  (setq org-startup-indented t)
  (setq org-todo-keywords
	'((sequence "TODO" "DOING" "|" "DONE" "CANCELED")))
  (setq org-capture-templates
	'(("t" "Todo" entry (file+headline "~/.emacs.d/org/inbox.org" "Tasks")
	   "* TODO %?\n  %i\n  %a")
	  ("j" "Journal" entry (file+datetree "~/.emacs.d/org/journal.org")
	   "* %?\nEntered on %U\n  %i\n  %a")))
  (setq org-default-notes-file "~/.emacs.d/org/inbox.org")
  (setq org-archive-location "~/.emacs.d/org/archives.org::* From %s")
  (setq org-agenda-files (list "~/.emacs.d/org/agenda.org"))

  ;; not display _ and ^ as sub/superscript
  (setq org-use-sub-superscripts nil)

  ;;src setting
  (setq org-src-fontify-natively t)

  :custom
  (org-support-shift-select 'alway)
  (org-babel-load-languages '((emacs-lisp . t)
			      (python . t)
			      (R . t)
			      (ein . t)))
  ;;local keybinding
  :bind
  (:map org-mode-map
	("C-c a" . org-agenda)
	("C-c c" . org-capture)
	("C-c C-r" . org-archive-subtree)
	("C-c t" . counsel-org-tag)
	("C-c l" . counsel-org-link))

  :hook
  (org-mode . (lambda ()
		(variable-pitch-mode 1)
		(visual-line-mode 1)
		(display-line-numbers-mode -1)
		(org-toggle-pretty-entities)
		;;(org-num-mode 1)
		)))

(use-package org-link-beautify
  :hook
  (org-mode . org-link-beautify-mode)
  :config
  (setq org-element-use-cache t)
  (define-key org-link-beautify-keymap (kbd "RET") 'org-open-at-point))

;;use org-superstar-mode to replace org-bullets
(use-package org-superstar
  :defer t
  :config
  (setq org-superstar-special-todo-items t)
  :hook
  (org-mode . org-superstar-mode)
  :custom
  (org-ellipsis "⤵"))

;;prettify-symbols-mode setting
(add-hook 'org-mode-hook 'prettify-symbols-mode)
(setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "λ")
				       ("#+END_SRC" . "λ")
				       ("#+begin_src" . "λ")
				       ("#+end_src" . "λ")
				       (">=" . "≥")
				       ("=>" . "⇨")
				       ("[-]" . "❍" )
				       ("[ ]" .  "☐")
				       ("[X]" . "☑" )))
(setq prettify-symbols-unprettify-at-point 'right-edge)

(use-package org-fancy-priorities
  :defer t
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("❗❗❗" "❗❗" "❗")))

;;Image drag-and-drop for org-mode
(use-package org-download
  :defer t
  :config
  (add-hook 'dired-mode-hook 'org-download-enable))

;;(use-package org-super-agenda)
(use-package org-graph-view
  :defer t
  :straight (org-graph-view :type git :host github
			    :repo "alphapapa/org-graph-view"))

;; This is an Emacs package that creates graphviz directed graphs from
;; the headings of an org file
(use-package org-mind-map
  :defer t
  :straight (org-mind-map :type git :host github
			    :repo "the-ted/org-mind-map")
  :config
  (setq org-mind-map-engine "dot")	; Default. Directed Graph
  ;; (setq org-mind-map-engine "neato")  ; Undirected Spring Graph
  ;; (setq org-mind-map-engine "twopi")  ; Radial Layout
  ;; (setq org-mind-map-engine "fdp")    ; Undirected Spring Force-Directed
  ;; (setq org-mind-map-engine "sfdp")   ; Multiscale version of fdp for the layout of large graphs
  )

;; perfectly alian English/CJK fonts in the same table
(use-package valign
  :hook (org-mode . valign-mode)
  :config
  (setq valign-fancy-bar t))

;; ==============================
;;             LaTeX           ;;
;; ==============================
(use-package tex ;;not auctex instead!
  :straight auctex
  :defer t
  :mode ("\\.tex\\'" . latex-mode)
  :config
  (setq TeX-auto-save t
	TeX-parse-self t)
  (setq-default TeX-engine 'xetex) ;;default engine
  (setq-default TeX-PDF-mode t)	   ;;PDF output
  (setq-default TeX-master nil)

  ;;auctex preview C-c C-p C-p (recommend use math-preview instead)
  (setq preview-pdf-color-adjust-method t)
  (set-default 'preview-scale-function 1.0) ;;preview scale
  ;;  (setq org-format-latex-options (plist-put org-format-latex-options :scale 3.0)) ;;preview in org-mode
  ;; (custom-set-faces
  ;;  '(preview-reference-face ((t (:background "gray" :foreground "black")))))

  ;;sync latex <-> pdf
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer) ;;auto revert PDF buffer

  ;;C-c C-v to sync forward, double click to sync backward
  (setq TeX-view-program-selection '((output-pdf "PDF Tools")
				     (output-dvi "DVI Viewer"))
	TeX-source-correlate-start-server t
	TeX-source-correlate-method 'auto) ;;Method to use for enabling forward and inverse search

  (add-hook 'LaTeX-mode-hook
	    (lambda ()
	      (projectile-mode -1)
	      (rainbow-delimiters-mode 1)
	      (visual-line-mode 1)
	      (LaTeX-math-mode 1)
	      (flycheck-mode -1)
	      (variable-pitch-mode 1)
	      (TeX-source-correlate-mode 1) ;;Needed to sync TeX and PDF
	      )))

;; Third-party preview for LaTeX
;; npm install -g git+https://gitlab.com/matsievskiysv/math-preview
;; There's also org-latex-impatient for org-mode
(use-package math-preview
  :custom
  (math-preview-command "/usr/local/bin/math-preview")
  (math-preview-scale 1.0)
  (math-preview-marks
   '(("\\begin{equation}" . "\\end{equation}")
     ("\\begin{equation*}" . "\\end{equation*}")
     ("\\[" . "\\]")
     ("\\(" . "\\)")
     ("$$" . "$$")
     ("$" . "$")
     ("\\begin{align}" . "\\end{align}")
     ("\\begin{align*}" . "\\end{align*}")))
  :bind
  (:map TeX-mode-map
	("C-c p p" . math-preview-at-point)
	("C-c p a" . math-preview-all)
	("C-c c p" . math-preview-clear-at-point)
	("C-c c a" . math-preview-clear-all)))

(use-package magic-latex-buffer
  :defer t
  :hook
  (LaTeX-mode . magic-latex-buffer)
  :config
  (setq magic-latex-enable-block-highlight nil
	magic-latex-enable-suscript t
	magic-latex-enable-pretty-symbols t
	magic-latex-enable-block-align nil
	magic-latex-enable-inline-image nil
	magic-latex-enable-minibuffer-echo nil))

;; Retrieve BibTeX entries
;; Call 'gscholar-bibtex' to retrieve BibTeX entries from Google
;; Scholar, ACM Digital Library, IEEE Xplore and DBLP.
;; (use-package gscholar-bibtex
;;   :defer t)

;; Reformat BibTeX using bibclean
;; (use-package bibclean-format
;;   :defer t
;;   :bind (:map bibtex-mode-map
;;               ("C-c f" . bibclean-format)))


;;; EMAIL CLIENT

;;read my blog to find how to install prerequisites
;; (use-package mu4e
;;   :defer t ;;will trigger org-mode
;;   :load-path "/usr/local/share/emacs/site-lisp/mu/mu4e"
;;   :config
;;   ;;Emacs Mail setting
;;   (setq user-mail-address "ying.rui@outlook.com")
;;   (setq user-full-name "Rui Ying/应锐")

;;   ;; SMTP settings:
;;   (setq send-mail-function 'smtpmail-send-it) ; should not be modified
;;   (setq smtpmail-smtp-user "ying.rui@outlook.com")
;;   (setq smtpmail-smtp-server "smtp.office365.com") ; host running SMTP server
;;   (setq smtpmail-smtp-service 587)	; SMTP service port number
;;   (setq smtpmail-stream-type 'starttls)	; type of SMTP connections to use
;;   (setq smtpmail-use-gnutls t)
;;   (setq mu4e-get-mail-command "mbsync -a")

;;   ;; Mail folders:
;;   (setq mu4e-maildir "~/Maildir")
;;   (setq mu4e-drafts-folder "/Drafts")
;;   (setq mu4e-sent-folder   "/Sent")
;;   (setq mu4e-trash-folder  "/Deleted Items")
;;   (setq mu4e-attachments-dir "~/Downloads")

;;   ;; Further customization:
;;   (setq m4e-html2text-command "w3m -T text/html" ; how to hanfle html-formatted emails
;; 	mu4e-update-interval 300 ; seconds between each mail retrieval
;; 	mu4e-headers-auto-update t    ; avoid to type `g' to update
;; 	mu4e-view-show-images t	      ; show images in the view buffer
;; 	mu4e-compose-signature-auto-include nil	; I don't want a message signature
;; 	mu4e-use-fancy-chars t)	  ; allow fancy icons for mail threads

;;   ;;Others
;;   (setq mu4e-main-buffer-hide-personal-addresses t)
;;   )


;;; PDF READER

;;allow you to view pdf continuously
(use-package pdf-continuous-scroll-mode
  :straight (pdf-continuous-scroll-mode :type git :host github
					:repo "dalanicolai/pdf-continuous-scroll-mode.el")
  :hook
  (pdf-view-mode-hook . pdf-continuous-scroll-mode))

;;read the documentation to find how to compile and pdf-tools first
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


;;; End

;; Back to normal GC level
(defun my-cleanup-gc ()
  "Clean up gc.  From user redguardtoo."
  (setq gc-cons-threshold 100000000)
  (setq gc-cons-percentage 0.1))

;; Collect gc during free time
(run-with-idle-timer 4 nil #'my-cleanup-gc)

(setq max-specpdl-size 32000
      max-lisp-eval-depth 16000)

(defun display-init-info ()
  "Print init time of Emacs, a wrapper of 'emacs-init-time."
  (interactive)
  (message
   (format "Start up in %.2fs with %d features and %d GC(s)"
	   (float-time (time-subtract after-init-time before-init-time))
	   (length features)
	   gcs-done)))
(add-hook 'after-init-hook #'display-init-info)

(provide 'init)
;;; Init.el ends here
