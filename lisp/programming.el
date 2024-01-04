;;; IDE FEATURES

;;Git + Emacs = boom!
(use-package magit
  :ensure-system-package git
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

;; code formatting, require third-party formatter
(use-package format-all
  :commands format-all-buffer)


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
  :commands (ess-r-mode)
  :config
  (setq ess-use-julia nil)
  (add-to-list 'auto-mode-alist '("\\.R\\'" . ess-r-mode)))

(with-eval-after-load 'ess-r-mode
  (defun ess-insert-pipe ()
    "Insert a R pipe (%>%)"
    (interactive)
    (just-one-space 1)
    (insert "%>%")
    (just-one-space 1)
    ;;(reindent-then-newline-and-indent)
    )

  ;; R official style
  (ess-set-style 'RRR 'quiet)
  (setq ess-nuke-trailing-whitespace-p 'ask)

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

  (define-key ess-r-mode-map (kbd "C-l") 'comint-clear-buffer)
  (define-key inferior-ess-r-mode-map (kbd "C-l") 'comint-clear-buffer) ;;inferior-* is the shell one
  (define-key ess-r-mode-map (kbd "M--") 'ess-insert-assign)
  (define-key inferior-ess-r-mode-map (kbd "M--") 'ess-insert-assign)
  (define-key ess-r-mode-map (kbd "M-p") 'ess-insert-pipe)
  (define-key inferior-ess-r-mode-map (kbd "M-p") 'ess-insert-pipe)
  (define-key inferior-ess-r-mode-map (kbd "C-p") 'comint-previous-input)
  ;; (define-key inferior-ess-r-mode-map (kbd "<up>") 'comint-previous-input)
  (define-key inferior-ess-r-mode-map (kbd "C-n") 'comint-next-input)
  ;; (define-key inferior-ess-r-mode-map (kbd "<down>") 'comint-next-input)
  )

(use-package julia-snail
  :hook
  (julia-mode . julia-snail-mode))

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

;; Toml
(use-package toml-mode
  :mode "\\.toml\\'")

;; julia-mode
(use-package julia-mode)

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


(use-package yasnippet
  :after elpy)

;; Language Server Protocol Implementation
;; require `epc` python libary
(use-package lsp-bridge
  :ensure-system-package (epc . "pip3 install epc")
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
  (acm-enable-copilot nil)
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


;; Install treesit C-library first: `brew install tree-sitter'
;; Automatically install and use tree-sitter major modes in Emacs 29+
(use-package treesit-auto
  :straight (:type git :host github :repo "renzmann/treesit-auto")
  :config
  (global-treesit-auto-mode)
  (setq treesit-auto-install 'prompt)
  (setq treesit-font-lock-level 4))

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
	      (local-set-key (kbd "C-x k") 'my-shell-exit-and-kill-buffer)))
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

(provide 'programming)
