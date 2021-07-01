;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;; Always remember that it is what you are editing, rather
;; than the editor, that is the important thing.
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


;;; FUNDEMENTAL

;;package manager
(require 'package)
(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

;;package.el
(when (< emacs-major-version 27)
  (package-initialize))

;; Native compilation support
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (progn
    (message "Native comp is enabled")
    (setq comp-deferred-compilation t)
    (setq native-comp-async-report-warning-errors nil)
    (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))
    (setq package-native-compile t)
    ))

;; Load path
;; Optimize: Force "lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("extra-lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))
(advice-add #'package-initialize :after #'update-load-path)
(update-load-path)

(setq custom-file (concat user-emacs-directory "/extra-lisp/custom.el"))
(load custom-file :noerror)

(setq gc-cons-percentage 0.6)
(setq read-process-output-max (* 1024 1024))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
;; (require 'bind-key)

(setq use-package-always-ensure t)

;; recompile outdated .elc file
;; (use-package auto-compile
;;   :init
;;   (auto-compile-on-load-mode)
;;   (auto-compile-on-save-mode))

;; Benchmark init time
;; Alternative: (setq use-package-verbose t)/(use-package-statistic-mode)
;; or simply `time emacs -e kill-emacs`
(use-package benchmark-init
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))


;;; EDITOR SECTION

;;Coding system
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;;We are lazy human :)
(fset 'yes-or-no-p 'y-or-n-p)

;;No more backup files~
(setq-default make-backup-files nil)

;;No more strange ring bell
(setq ring-bell-function 'ignore)

;;flyspell setting
(add-hook 'text-mode-hook 'flyspell-mode)
(setq-default ispell-program-name "aspell") ;;depends on aspell in the path
(setq ispell-local-dictionary "en_GB")
(setq ispell-extra-args '("--sug-mode=fast" "--lang=en_GB" "--camel-case" "--run-together"))

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
;;   (setq evil-disable-insert-state-bindings t)
;;   )

;; >>> OPTION2: viper-mode (built-in vim-like)
;; (use-package viper
;;   :ensure nil
;;   :init (setq viper-mode t)
;;   )

;; >>> OPTION3: god-mode (remove prefix key)
;; (use-package god-mode
;;   :init
;;   (god-mode)
;;   )

(use-package recentf
  :ensure nil
  :config
  (setq-default
   recentf-max-saved-items 30
   recentf-exclude `("/tmp/", (concat package-user-dir "/.*-autoloads\\.el\\'")))
  (global-set-key (kbd "<f3>") 'recentf-open-files) ;;use C-c C-r to call counsel-recentf
  :hook
  (after-init . recentf-mode))

;;use undo-tree-visualize to show history
(use-package undo-tree
  :defer t
  :hook
  (after-init . global-undo-tree-mode)
  :bind
  ("C-c u" . undo-tree-visualize)
  )


;;; TERMINAL, COMPLETION, LINT, SNIPPET

;; Running Shells in Emacs
;; Tips: you can use M-r to search in shell history
;; History references like '!' (reference), ‘!!’ (last cmd) and ‘^’ (substituion, e.g., ^a^b) are supported
;; If you don't know the history reference, use C-c C-l to list all (will work for most comint buffers)
(use-package shell
  :ensure nil
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
  ("C-x t" . shell)
  (:map shell-mode-map
	("<up>" . comint-previous-input)
	("C-p" . comint-previous-input)
	("<down>" . comint-next-input)
	("C-n" . comint-next-input)
	("C-l" . comint-clear-buffer)
	("SPC" . comint-magic-space))) ;;magically expand history reference, <TAB> also works


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
                       (company-mode +1)))))

;;A fuzzy matching of company
(use-package company-flx
  :hook
  (company-mode . company-flx-mode)
  )

;;simple and fast sorting and filtering framework for comppany
(use-package company-prescient
  :hook (company-mode . company-prescient-mode)
  :config
  (setq prescient-filter-method '(literal regexp initialism)))

;; (use-package company-quickhelp
;;   :config
;;   (add-hook 'company-quickhelp-mode-hook 'python-mode)
;;   ;(company-quickhelp-mode 1) ;;globally true
;;   (eval-after-load 'company
;;     '(define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin)))

;;Alternative to company-quickhelp
;; (use-package company-posframe
;;   :config
;;   (company-posframe-mode 1)
;;   )

(use-package company-box
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-icons-alist 'company-box-icons-images))

;;To specify new version of git on remote machine so I can run magit locally
;;add ~/.ssh/config and ~/.ssh/known_hosts first
;;then ssh-keygen -t rsa => ssh-copy-id name@host_name
(use-package tramp
  :defer t
  :if (memq system-type '(gnu/linux darwin))
  :config
  (add-to-list 'tramp-remote-path "/usr/local/bin/git")
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  ;;tramp mode to cache password
  (setq password-cache-expiry nil))

(use-package counsel-tramp
  :defer 1
  :after (counsel tramp)
  :config
  (setq counsel-tramp-custom-connections '(/scp:mogu@almond.ggy.bris.ac.uk:/home/mogu/cgenie.muffin/))
  (setq tramp-default-method "scp")
  (setq make-backup-files nil)
  (setq create-lockfiles nil)
  (define-key global-map (kbd "C-c s") 'counsel-tramp)
  )

;;visit https://github.com/jacktasia/dumb-jump to see more alternative ways
;;for example, TAGS system and so on
;;======================================================================
;;depends on external program The-Silver-Searcher/ripgrep and emacs package ag/rg
;;======================================================================
(use-package dumb-jump
  :defer 4
  :requires (ag ripgrep rg)
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-default-project "~/cgenie.muffin")
  (add-to-list 'auto-mode-alist '("\\.config\\'" . shell-script-mode)))

(use-package which-key
  :hook
  (after-init . which-key-mode))

;; on-the-fly syntax checker,  use C-c ! as prefix, e.g., C-c ! v to verify the checkers
;; use M-g n/p to navigate error, or use C-c e (counsel-flycheck)
(use-package flycheck
  :hook
  (prog-mode . flycheck-mode)
  :config
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  ;; python checker
  ;; conda install -c anaconda flake8
  (add-to-list 'flycheck-disabled-checkers 'python-pylint)
  (add-to-list 'flycheck-disabled-checkers 'python-mypy)
  (add-to-list 'flycheck-disabled-checkers 'python-pyright)
  (add-to-list 'flycheck-disabled-checkers 'python-pycompile)
  ;; shell checker
  ;; conda install -c conda-forge shellcheck
  (add-to-list 'flycheck-disabled-checkers 'sh-posix-dash)
  (add-to-list 'flycheck-disabled-checkers 'sh-posix-bash)
  ;; Elisp checker
  (add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc)
  :custom
  (flycheck-python-flake8-executable "python3"))

(use-package flycheck-inline
  :hook
  (flycheck-mode . flycheck-inline-mode)
  )

(use-package yasnippet
  :ensure yasnippet-snippets  ;; Collection of snippets
  :hook (after-init . yas-global-mode)
  )

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
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "C-x b") 'ivy-switch-buffer)
  (global-set-key (kbd "C-c v") 'ivy-push-view)
  (global-set-key (kbd "C-c V") 'ivy-pop-view)
  (setq ivy-wrap t)
  (setq ivy-height 9)
  ;;  (setq ivy-format-function 'ivy-format-function-line)
  )

(use-package counsel
  :defer 1
  :after ivy
  :config
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
  :bind
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
  )

;;sorting and filtering framework for ivy
(use-package ivy-prescient
  :hook (ivy-mode . ivy-prescient-mode)
  :config
  (setq ivy-prescient-sort-commands t
	ivy-prescient-enable-sorting nil
	ivy-prescient-retain-classic-highlighting t)
  )

;; Project management tool
(use-package projectile
  :after ivy
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'ivy)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  )

;;Faster cursor movement - go to anywhere
(use-package avy
  :defer 1
  :config
  (global-set-key (kbd "C-\"") 'avy-goto-char)  ;;input one character
  (global-set-key (kbd "C-'") 'avy-goto-char-2) ;;input two characters
  (global-set-key (kbd "M-g w") 'avy-goto-word-1)
  (global-set-key (kbd "M-g l") 'avy-goto-line))

(use-package helpful
  :defer 2
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  )

(use-package ace-window
  :defer 4
  :config
  (global-set-key (kbd "M-o") 'ace-window))

;;Another interesting package for fuzzy finding
;; (use-package affe
;;   :after orderless
;;   :config
;;   ;; Configure Orderless
;;   (setq affe-regexp-function #'orderless-pattern-compiler
;;         affe-highlight-function #'orderless-highlight-matches)

;;   ;; Manual preview key for `affe-grep'
;;  (consult-customize affe-grep :preview-key (kbd "M-.")))


;;; KEYBINDING

;; Set meta command for Mac OS
;; If you are using a external Windows keyboard, remeber to choose
;; USB keyboard in Preference -> Keyboard -> modify keyboard -> select keyboard
(if (eq system-type 'darwin)
    (setq mac-command-modifier 'meta))

(global-set-key (kbd "C-x k") 'kill-this-buffer)

(defun open-init-file()
  "Open the init.el file under .emacs.d directory."
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "<f2>") 'open-init-file)

(defun select-current-line ()
  "Select the current line."
  (interactive)
  (move-beginning-of-line 1)
  (push-mark nil nil t)
  ;;(forward-line 1)
  (end-of-line 1))
(global-set-key (kbd "C-l") 'select-current-line)

;; Martset
;; In case you can't use C-SPEC to do markset, change it to C-j
;; (global-set-key (kbd "C-j") 'set-mark-command)
;; C-x C-x -> set mark and go back
;; C-x h to select all

;; a human-friendly keymap of built-in code-folding package
;; alternatives: vimish-fold, Origami
(add-hook 'prog-mode-hook #'hs-minor-mode)
(global-set-key (kbd "<f5>") 'hs-toggle-hiding)

;; adjust font size
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(setq-default text-scale-mode-step 1.1)

;; find and replace
(global-set-key (kbd "C-c h") 'query-replace)

;; select one and edit all (https://github.com/victorhge/iedit)
;; iedit is also dependency of lispy, use M-i to toggle 
(use-package iedit
  :bind
  ("M-i" . iedit-mode)
  )

;;; UI & APPEARANCE

;;If you are running Emacs in MacOS, then I recommend you using
;;Emacs-mac <--with-no-title-bars> which improves GUI performance a lot
;;Also: for the Emacs-mac you can swipe between buffer by using two fingers (cool!)
(when (eq system-type 'darwin)
  (progn
    (setq dired-use-ls-dired nil) ;;to avoid error "ls does not support --dired" in MacOS
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)) ;;same title bar color
    (add-to-list 'default-frame-alist '(ns-appearance . light))))

;;Auto-max the frame at startup
(defun auto-max-frame()
  "Maxize/full screen the frame according to OS type"
  (interactive)
  (if (eq system-type 'darwin)
      (toggle-frame-maximized)
    (toggle-frame-fullscreen)))
(add-hook 'after-init-hook #'auto-max-frame)

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

(setq x-underline-at-descent-line t)

;;Display line number
(global-display-line-numbers-mode t) ;;the linum-mode has been obsolete
(setq display-line-numbers-width 0)

;;Disable line number for certain modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;turn off electric-indent-mode but use aggressive-indent-mode
(electric-indent-mode 1)
;; (use-package aggressive-indent
;;   :defer t
;;   :hook
;;   (prog-mode . aggressive-indent-mode))

(setq frame-inhibit-implied-resize nil)

;; (use-package mini-frame
;;   :config
;;   (setq mini-frame-show-parameters
;;         `((left . 0.5)
;;           (top . 1.0) (width . 1.0)
;;           (height . 8)
;;           (left-fringe . 12)
;;           (right-fringe .12)
;;           (child-frame-border-width . 0)
;;           (internal-border-width . 0)
;; 	  ))
;;   (setq resize-mini-frames t)
;;   (mini-frame-mode t)
;;   )

;;-----------Dired enhancement-------------
;; (use-package dired-hacks-utils
;;   :defer t
;;   :hook
;;   (dired-mode . dired-utils-format-information-line-mode)
;;   )

(setq dired-listing-switches "-alFh")

(use-package dired-ranger
  :defer t
  :hook
  (dired-mode . dired-utils-format-information-line-mode))

;; (use-package dired-collapse
;;   :hook
;;   (dired-mode . dired-collapse-mode))

;;--------------------------------------------------
;; Matching parenthesis
;;--------------------------------------------------

;;Showing matching parentheses (built-in)
;;  (show-paren-mode t)
;; (setq show-paren-delay 0)
;; (setq show-paren-style 'parenthesis) ;;Options: parenthesis/expression/mixed

(use-package highlight-parentheses
  :defer t
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
                            (?\{ . ?\})
                            ))

;; use lispy-mode (a vi-like editing) for lisp parentheses
;; remove electric-pair-mode first
(add-hook 'emacs-lisp-mode-hook (lambda()
				  (electric-pair-local-mode -1)))

;; >>> Basic lispy usage:
;; jkhl to move, f/b to foward/backward level
;; c to copy, m to mark, e to evaluate, d to swith parenthesis side (C-d to delete)
;; >/< to slurp/barf: push out/pull in
;; w/s to move marked regions up/down
;; M-j to split, + to join

(use-package lispy
  :hook
  (emacs-lisp-mode . lispy-mode)
  :bind
  (:map lispy-mode-map
	("M-o" . ace-window)))

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
;;   :defer t
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
  :defer t
  :hook
  (prog-mode . rainbow-delimiters-mode)
  )

;; (use-package spaceline
;;  :init
;;  (require 'spaceline-config)
;;  :config
;;  (spaceline-emacs-theme))

(use-package mood-line
  :defer t
  :hook
  (after-init . mood-line-mode))

;; (use-package doom-modeline
;;   ;;right fringe cut-off issue should relate to font size
;;   ;;Use cnfont-decrease-size or see more methods in 
;;   ;;https://github.com/hlissner/doom-emacs/blob/develop/modules/ui/modeline/README.org#the-right-side-of-the-modeline-is-cut-off
;;   :defer t
;;   :after all-the-icons
;;   :hook (after-init . doom-modeline-mode)
;;   :config
;;   (setq doom-modeline-window-width-limit fill-column)
;;   (setq doom-modeline-icon (display-graphic-p))
;;   (setq doom-modeline-major-mode-icon t)
;;   (setq doom-modeline-enable-word-count nil)
;;   (setq all-the-icons-scale-factor 1.0)
;;   )

(use-package nyan-mode
  :defer t
  :hook
  (doom-modeline-mode . nyan-mode))

;; (use-package dashboard
;;   :if (< (length command-line-args) 2)
;;   :config
;;   (dashboard-setup-startup-hook)
;;   (setq dashboard-set-init-info t)
;;   (setq dashboard-banner-logo-title "Happiness is everything - Rui")
;;   ;;    (setq dashboard-startup-banner 3)
;;   (setq dashboard-startup-banner "~/.emacs.d/fancy-splash/world.png")
;;   (setq dashboard-center-content t)
;;   (setq dashboard-items '((recents  . 3))) ;;add org-agenda could slow start-up speed
;;   (setq dashboard-set-heading-icons t)
;;   (setq dashboard-set-file-icons t)
;;   (setq dashboard-set-navigator t)
;;   (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))) ;; show Dashboard in frames created with emacsclient -c
;;   (setq dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)
;;   )

(use-package highlight-indent-guides
  :defer t
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
					;  (set-face-background 'highlight-indent-guides-character-face "dimgrey")
  :custom
  (highlight-indent-guides-method 'character)
  )

(use-package highlight-symbol
  :defer t
  ;; An alternative package is highlight-thing
  :bind
  ("C-<f9>" . highlight-symbol)
  ("<f9>" . highlight-symbol-next)
  ("S-<f9>" . highlight-symbol-prev)
  ("M-<f9>" . highlight-symbol-query-replace)
  )

(use-package minimap
  :defer t
  :custom
  (minimap-window-location 'right)
  (minimap-width-fraction 0.05)
  (minimap-minimum-width 15)
  :bind
  ("<f6>" . minimap-mode)
  )

;;(use-package neotree
;;  :config
;;  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
;;  (global-set-key [f8] 'neotree-toggle)
;;  )

(use-package treemacs
  :defer t
  :bind
  ("<f8>" . treemacs)
  )

(use-package treemacs-all-the-icons
  :defer t
  :requires
  (treemacs all-the-icons)
  :config
  (treemacs-load-theme "all-the-icons")
  )

(use-package all-the-icons-dired
  :defer t
  ;;need to run all-the-icons-install-fonts first to avoid grabled icon
  :requires all-the-icons
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
   '((ivy-rich-switch-buffer-size (:align right)))))

(use-package all-the-icons-ivy-rich
  :after ivy-rich
  :config
  (setq all-the-icons-ivy-rich-icon-size 1.0)
  (setq inhibit-compacting-font-caches t)
  (all-the-icons-ivy-rich-mode t)
  )

;; (use-package ivy-posframe ;;center your selection candidate box
;;   :config
;;   (setq ivy-posframe-display-functions-alist
;; 	'((complete-symbol . ivy-posframe-display-at-point)
;; 	  (swiper . ivy-posframe-display-at-window-center)
;;           (counsel-M-x     . ivy-posframe-display-at-window-center)
;;           (t               . ivy-posframe-display)))
;;   (setq ivy-posframe-height-alist '((swiper . 15)
;;                                     (t      . 10)))
;;   (ivy-posframe-mode 1))

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
  (centaur-tabs-change-fonts "Roboto Mono" 120)
  :bind
  ("M-<left>" . centaur-tabs-backward)
  ("M-<right>" . centaur-tabs-forward)
  :hook
  (dired-mode . centaur-tabs-local-mode)
  (after-init . centaur-tabs-mode)
  )

(use-package visual-fill-column
  :defer t
  :hook
  (visual-line-mode . visual-fill-column-mode)
  :custom
  (visual-fill-column-center-text t)
  (visual-fill-column-width 100)
  )

;;press your keyboard fast and hard !!!
(use-package power-mode
  :defer t
  :load-path "extra-lisp/"
  )

;; to display ^L page break
(use-package form-feed
  :hook
  (emacs-lisp-mode . form-feed-mode)
  )

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
;;Chinese font: Wenquanyi Micro Hei Mono/Sarasa UI SC Mono/Noto Sans CJK SC Mono (work perfectly with Iosevka/Inconsolata)
;;Variable-pitch font, ETBembo/New York
;;Unicode: Symbola

(defun init-font()
  "Set English and CJK font for Emacs."
  (interactive)
  ;; English font
  (if (display-graphic-p)
      (progn
	;; English font
        (set-face-attribute 'default nil :font (format "%s:pixelsize=%d" "Iosevka" 16))
        ;; CJK font
        (dolist (charset '(kana han symbol cjk-misc bopomofo))
          (set-fontset-font (frame-parameter nil 'font)
                            charset
                            (font-spec :family "Noto Sans Mono CJK SC"))))
    ))

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
  (add-hook 'after-init-hook 'init-font)
  )

;;Install themes
(use-package base16-theme :defer t)
(use-package color-theme-sanityinc-tomorrow :defer t)
(use-package gruvbox-theme :defer t)
(use-package tao-theme :defer t)
(use-package humanoid-themes :defer t)
(use-package twilight-bright-theme :defer t)
(use-package ample-theme :defer t) ;;ample flat is a good option for dark theme
(use-package eziam-theme :defer t) ;;almost perfect light theme
(use-package spacemacs-common
  :defer t
  :ensure spacemacs-theme)

(use-package doom-themes
  :defer t
  ;; :config
  ;; ;;treemacs setting
  ;; (setq doom-themes-treemacs-enable-variable-pitch nil)
  ;; (setq doom-themes-treemacs-theme "doom-color")
  ;; (doom-themes-treemacs-config)  
  ;; ;; Corrects (and improves) org-mode's native fontification.
  ;; (doom-themes-org-config)
  )

(setq custom-safe-themes t)
;;the core of this file, use C-c t to change
;;(load-theme 'humanoid-dark t)
;;(load-theme 'doom-dark+ t)
(load-theme 'doom-one t)
;;(load-theme 'doom-vibrant t)

;;Transprancy setting
(set-frame-parameter (selected-frame) 'alpha '(97 100))
(add-to-list 'default-frame-alist '(alpha 97 100))

;;Font Setting
(setq inhibit-compacting-font-caches t)

;;Varialble/fixed pictch font setting, essential for org-mode
(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "Cormorant Garamond" :height 230))))
 '(fixed-pitch ((t ( :family "Monaco" :height 160)))))

(custom-theme-set-faces
 'user
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

;;Unicode font setting
(when (member "Symbola" (font-family-list))
  (set-fontset-font "fontset-default" nil
		    (font-spec :size 20 :name "Symbola")))

(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))


;;; PROGRAMMING LANGUAGES & LSP

;;==============================
;;            Eglot           ;;
;;==============================
;;eglot can work with tramp-mode, but you should install
;;your server-programs on remote, not local
(use-package eglot
  :defer t
  :config
  (add-hook 'eglot-managed-mode-hook (lambda () (flymake-mode -1))) ;;Decouple flymake and eglot
  ;;============================================
  ;;make sure every command works separately in shell environment. Note R can be tricky in zsh due to the built-in command "r"
  (set 'ad-redefinition-action 'accept)
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) ("clangd"))) ;;install clangd first
  (add-to-list 'eglot-server-programs '(f90-mode . ("fortls"))) ;;pip3 install fortran-language-server
  (add-to-list 'eglot-server-programs '((LaTeX-mode tex-mode context-mode texinfo-mode bibtex-mode) ;;use tex-lab or digestif as server
					. ("texlab")))
  (add-to-list 'eglot-server-programs '(python-mode . ("pylsp"))) ;;pip3 install python-lsp-server
  ;;jupterlab has some experimental lsp server, install and change it above: pip3 install git+https://github.com/krassowski/python-language-server.git@main
  (add-to-list 'eglot-server-programs '(ess-r-mode . ("R" "--slave" "-e" "languageserver::run()"))) ;;install.packages("languageserver")
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
  (defun python-run-current-line()
    "a wrapper of python-shell-send-statement"
    (interactive)
    (python-shell-send-statement)
    (forward-line))
  (define-key python-mode-map (kbd "C-<return>") 'python-run-current-line)
  (define-key inferior-python-mode-map (kbd "C-l") 'comint-clear-buffer)
  (define-key inferior-python-mode-map (kbd "<up>") 'comint-previous-input)
  (define-key inferior-python-mode-map (kbd "C-p") 'comint-previous-input)
  (define-key inferior-python-mode-map (kbd "<down>") 'comint-next-input)
  (define-key inferior-python-mode-map (kbd "C-n") 'comint-next-input)
  )


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

(use-package ein
  ;;ein-babel see the init-org.el
  :defer 1
  :config
  (setq ein:use-company-backend t)
  (setq ein:worksheet-enable-undo t)
  (setq ein:output-area-inlined-images t)
  (add-hook 'poly-ein-mode-hook 'elpy-enable)
  (add-hook 'poly-ein-mode-hook (lambda()
				  (display-line-numbers-mode nil))) ;;avoid grabled line-number
  (with-eval-after-load 'ein-notebook
    (define-key ein:notebook-mode-map "\C-c\C-d" 'ein:worksheet-delete-cell))
  )

(use-package elpy ;;completion system for EIN
  :defer t)

;;==============================
;;           Rlang            ;;
;;==============================
;; require ESS installed
;;Lazy load ess-r-mode (ESS doesn't like use-package pretty much)
(unless (package-installed-p 'ess)
  (package-refresh-contents)
  (package-install 'ess))

(use-package ess
  :defer t)

(add-to-list 'auto-mode-alist '("\\.R\\'" . ess-r-mode))
(with-eval-after-load 'ess-r-mode
  (defun ess-insert-pipe()
    "Insert a R pipe (%>%)"
    (interactive)
    (just-one-space 1)
    (insert "%>%")
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
  (define-key inferior-ess-r-mode-map (kbd "up") 'comint-previous-input)
  (define-key inferior-ess-r-mode-map (kbd "C-n") 'comint-next-input)
  (define-key inferior-ess-r-mode-map (kbd "<down>") 'comint-next-input)
  )

;;C-c C-a to turn on csv-align-fields
(use-package csv-mode
  :defer t
  :mode
  "\\.csv\\'"
  "\\.CSV\\'"
  )

;;display color of RGB code
(use-package rainbow-mode
  :defer t
  :after ess
  :hook ess-r-mode
  )

;;==============================
;;           Matlab           ;;
;;==============================
;; cd /path/to/matlab-emacs -> make
;; Homepage: https://sourceforge.net/p/matlab-emacs/src/ci/documentation/tree/
(use-package matlab-mode
  :defer t
  :mode "\\.[mM]\\'"
  )

;; ==============================
;;            Markdown         ;;
;; ==============================

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :hook
  (markdown-mode . (lambda()
		     (display-line-numbers-mode -1)
		     (visual-line-mode 1)))
  )

;;pip install grip first
(use-package grip-mode
  :bind (:map markdown-mode-command-map
              ("g" . grip-mode))
  :hook (markdown-mode . grip-mode)
  :config
  ;;create your personal access token, then config
  ;;your github username and token in "~/.authinfo.gpg"
  ;;DO NOT input your password here!
  (require 'auth-source)
  (let ((credential (auth-source-user-and-password "api.github.com")))
    (setq grip-github-user (car credential)
          grip-github-password (cadr credential)))  
  )

;;add table of content for md/org
;;Add :TOC: tag for org (C-c C-c) and <-- :TOC: --> for md
;;then toc-org-insert-toc
(use-package toc-org
  :hook
  (markdown-mode . toc-org-mode)
  (org-mode . toc-org-mode)
  :config
  (global-set-key (kbd "\C-c\C-o") 'toc-org-markdown-follow-thing-at-point)
  (add-to-list 'org-tag-alist '("TOC" . ?T))
  )

;; ==============================
;;           Org-mode          ;;
;; ==============================
(use-package org
  :ensure nil
  :defer t
  :after counsel
  :config
  (setq org-startup-indented t)
  (setq org-todo-keywords
	'((sequence "TODO" "DOING"  "|" "DONE" "CANCELED")))
  (add-hook 'org-mode-hook 'org-toggle-pretty-entities)
  (setq org-capture-templates
	'(("t" "Todo" entry (file+headline "~/.emacs.d/org/inbox.org" "Tasks")
	   "* TODO %?\n  %i\n  %a")
	  ("j" "Journal" entry (file+datetree "~/.emacs.d/org/journal.org")
           "* %?\nEntered on %U\n  %i\n  %a")))
  (setq org-default-notes-file "~/.emacs.d/org/inbox.org")
  (setq org-archive-location "~/.emacs.d/org/archives.org::* From %s")
  (setq org-agenda-files (list  "~/.emacs.d/org/agenda.org"))

  ;;src setting
  (setq org-src-fontify-natively t)
  ;;babel setting
  (org-babel-do-load-languages 'org-babel-load-languages
			       '((emacs-lisp . t)
				 (python . t)
				 (R . t)
				 (ein . t)
				 ))
  :custom
  (org-support-shift-select 'alway)
  ;;local keybinding
  :bind
  (:map org-mode-map
	("C-c a" . org-agenda)
	("C-c c" . org-capture)
	("C-c C-r" . org-archive-subtree)
	("C-c t" . counsel-org-tag)
	("C-c l" . counsel-org-link))
  :hook
  (org-mode . (lambda()
		(variable-pitch-mode 1)
		(visual-line-mode 1)
		(display-line-numbers-mode -1)
		(flyspell-mode 1)
		;;(org-num-mode 1)
		))
  )

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
  :load-path "extra-lisp/"
  )

;; This is an Emacs package that creates graphviz directed graphs from
;; the headings of an org file
(use-package org-mind-map
  :defer t
  :load-path "extra-lisp/"
  :config
  (setq org-mind-map-engine "dot")       ; Default. Directed Graph
  ;; (setq org-mind-map-engine "neato")  ; Undirected Spring Graph
  ;; (setq org-mind-map-engine "twopi")  ; Radial Layout
  ;; (setq org-mind-map-engine "fdp")    ; Undirected Spring Force-Directed
  ;; (setq org-mind-map-engine "sfdp")   ; Multiscale version of fdp for the layout of large graphs
  )

(use-package valign
  :hook (org-mode . valign-mode)
  :config
  (setq valign-fancy-bar t))

;; ==============================
;;             LaTeX           ;;
;; ==============================
(use-package tex ;;not auctex instead!
  :ensure auctex
  :defer t
  :mode ("\\.tex\\'" . latex-mode)
  :config
  (setq TeX-auto-save t
        TeX-parse-self t)
  (setq-default TeX-engine 'xetex) ;;default engine
  (setq-default TeX-PDF-mode t)	   ;;PDF output
  (setq-default TeX-master nil)

  ;;Preview latex C-c C-p C-p
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
	      (rainbow-delimiters-mode 1)
              (visual-line-mode -1)
	      (visual-fill-column-mode -1)
	      (LaTeX-math-mode 1)
	      (display-line-numbers-mode 1)
	      (flyspell-mode 1)
	      (flycheck-mode -1)
	      ;;(variable-pitch-mode 1)
	      (TeX-source-correlate-mode 1) ;;Needed to sync TeX and PDF
	      ))
  )

(use-package magic-latex-buffer
  :defer t
  :hook
  (LaTeX-mode . magic-latex-buffer)
  :config
  (setq magic-latex-enable-block-highlight nil
	magic-latex-enable-suscript        t
	magic-latex-enable-pretty-symbols  t
	magic-latex-enable-block-align     nil
	magic-latex-enable-inline-image    nil
	magic-latex-enable-minibuffer-echo nil))

;; Retrieve BibTeX entries
;; Call 'gscholar-bibtex' to retrieve BibTeX entries from Google
;; Scholar, ACM Digital Library, IEEE Xplore and DBLP.
(use-package gscholar-bibtex
  :defer t)

;; Reformat BibTeX using bibclean
(use-package bibclean-format
  :defer t
  :bind (:map bibtex-mode-map
              ("C-c f" . bibclean-format)))


;;; EMAIL CLIENT

;;read my blog to find how to install prerequisites
(use-package mu4e
  :defer t ;;will trigger org-mode
  :load-path "/usr/local/share/emacs/site-lisp/mu/mu4e"
  :config
  ;;Emacs Mail setting
  (setq user-mail-address "ying.rui@outlook.com")
  (setq user-full-name "Rui Ying/应锐")

  ;; SMTP settings:
  (setq send-mail-function 'smtpmail-send-it) ; should not be modified
  (setq smtpmail-smtp-user "ying.rui@outlook.com")
  (setq smtpmail-smtp-server "smtp.office365.com") ; host running SMTP server
  (setq smtpmail-smtp-service 587)	; SMTP service port number
  (setq smtpmail-stream-type 'starttls)	; type of SMTP connections to use
  (setq smtpmail-use-gnutls t)
  (setq mu4e-get-mail-command "mbsync -a")

  ;; Mail folders:
  (setq mu4e-maildir "~/Maildir")
  (setq mu4e-drafts-folder "/Drafts")
  (setq mu4e-sent-folder   "/Sent")
  (setq mu4e-trash-folder  "/Deleted Items")
  (setq mu4e-attachments-dir "~/Downloads")

  ;; Further customization:
  (setq m4e-html2text-command "w3m -T text/html" ; how to hanfle html-formatted emails
	mu4e-update-interval 300 ; seconds between each mail retrieval
	mu4e-headers-auto-update t    ; avoid to type `g' to update
	mu4e-view-show-images t	      ; show images in the view buffer
	mu4e-compose-signature-auto-include nil	; I don't want a message signature
	mu4e-use-fancy-chars t)	  ; allow fancy icons for mail threads

  ;;Others
  (setq mu4e-main-buffer-hide-personal-addresses t)
  )

;;; PDF READER

;;allow you to view pdf continuously
(use-package pdf-continuous-scroll-mode
  :defer t
  :load-path "extra-lisp/"
  :hook
  (pdf-view-mode-hook . pdf-continuous-scroll-mode))

;; one way to download using qulepa, but too slow and annoying for me
;; (use-package pdf-continuous-scroll-mode
;;   :defer t
;;   :quelpa
;;   (pdf-continuous-scroll-mode :fetcher github
;; 			      :repo "dalanicolai/pdf-continuous-scroll-mode.el"))

;;read the documentation to find how to compile and pdf-tools first
(use-package pdf-tools
  :defer t
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
  (pdf-view-mode-hook . (lambda () (display-line-numbers-mode -1)))
  (pdf-view-mode-hook . pdf-tools-enable-minor-modes)
  :bind
  (:map pdf-view-mode-map
	("C-s" . isearch-forward-regexp)
	("j" . pdf-view-next-line-or-next-page)
	("k" . pdf-view-previous-line-or-previous-page))
  )


;;; End

(defun my-cleanup-gc ()
  "Clean up gc."
  (setq gc-cons-threshold  (* 128 1024 1024)) ; 64M
  (setq gc-cons-percentage 0.3)		      ; original value
  (garbage-collect))
(run-with-idle-timer 4 nil #'my-cleanup-gc)

(setq max-specpdl-size 32000
      max-lisp-eval-depth 16000)

(defun display-init-info()
  "Print init time of Emacs, a wrapper of emacs-init-time"
  (interactive)
  (message
   (format "Start up in %.2fs with %d features and %d GCs"
	   (float-time (time-subtract after-init-time before-init-time))
	   (length features)
	   gcs-done)))
(add-hook 'after-init-hook #'display-init-info)

(provide 'init)
;;; init.el ends here
