;;Highlight current line, based on default hl-line-mode
(use-package lin
  :hook
  (prog-mode . lin-mode)
  (text-mode . lin-mode))

;;Display line number
(global-display-line-numbers-mode t) ;;the linum-mode has been obsolete
(setq display-line-numbers-width 0)

;; show tree-like structure of current position
(use-package breadcrumb
  :straight (breadcrumb :type git
			:host github
			:repo "joaotavora/breadcrumb")
  :hook
  (prog-mode . breadcrumb-mode))

;; quick selection of the region
(use-package expreg
  :bind
  ("C-=" . expreg-expand)
  ("C--" . expreg-contract))

(use-package indent-bars
  :if window-system
  :straight (indent-bars :type git :host github :repo "jdtsmith/indent-bars")
  :custom
  (indent-bars-treesit-support nil)
  (indent-bars-no-descend-string t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  :hook ((prog-mode) . indent-bars-mode)
  :config
  (setq
   indent-bars-color '(highlight :face-bg t :blend 0.3)
   indent-bars-pattern " . . . . ." ; play with the number of dots for your usual font size
   indent-bars-width-frac 0.25
   indent-bars-pad-frac 0.1)
  )

;; ensure pasted (yanked) text has the correct indentation level.
(use-package yank-indent
  :straight (:host github :repo "jimeh/yank-indent")
  :config (global-yank-indent-mode t))

;; stop automatically indents the line
(electric-indent-mode nil)

;; Showing matching parentheses (built-in)
(use-package highlight-parentheses
  :hook
  (after-init . global-highlight-parentheses-mode)
  :config
  (setq highlight-parentheses-highlight-adjacent t))

;; insert matching pair of delimiters
(electric-pair-mode t)
(setq electric-pair-pairs '((?\" . ?\")
			    (?\` . ?\`)
			    (?\( . ?\))
			    (?\{ . ?\})))


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

;; minimal columns for Emacs to split window horizontally
(setq split-width-threshold 130)

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

;;; Auto-save buffer
(use-package real-auto-save
  :hook
  (prog-mode . real-auto-save-mode)
  (text-mode . real-auto-save-mode)
  :custom
  ;; configure time gap (in sec)
  (real-auto-save-interval 1))

;; Automatically add spacing around operators
;; use C-v to next page
(use-package electric-operator
  :hook
  (python-mode . electric-operator-mode)
  (ess-r-mode . electric-operator-mode))

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

;; (use-package pixel-scroll
;;   :straight (:type built-in)
;;   :if (> emacs-major-version 28)
;;   :bind
;;   ([remap scroll-up-command]   . pixel-scroll-interpolate-down)
;;   ([remap scroll-down-command] . pixel-scroll-interpolate-up)
;;   :custom
;;   (pixel-scroll-precision-interpolate-page t)
;;   :init
;;   (pixel-scroll-precision-mode 1))

;; ---Edit keybinding style---
;; A better undo/redo mode
(use-package vundo
  :commands (vundo)
  :config
  ;; Take less on-screen space.
  (setq vundo-compact-display t)

  ;; Better contrasting highlight.
  (custom-set-faces
   '(vundo-node ((t (:foreground "#808080"))))
   '(vundo-stem ((t (:foreground "#808080"))))
   '(vundo-highlight ((t (:foreground "#FFFF00")))))

  ;; Use `HJKL` VIM-like motion, also Home/End to jump around.
  (define-key vundo-mode-map (kbd "l") #'vundo-forward)
  (define-key vundo-mode-map (kbd "<right>") #'vundo-forward)
  (define-key vundo-mode-map (kbd "h") #'vundo-backward)
  (define-key vundo-mode-map (kbd "<left>") #'vundo-backward)
  (define-key vundo-mode-map (kbd "j") #'vundo-next)
  (define-key vundo-mode-map (kbd "<down>") #'vundo-next)
  (define-key vundo-mode-map (kbd "k") #'vundo-previous)
  (define-key vundo-mode-map (kbd "<up>") #'vundo-previous)
  (define-key vundo-mode-map (kbd "<home>") #'vundo-stem-root)
  (define-key vundo-mode-map (kbd "<end>") #'vundo-stem-end)
  (define-key vundo-mode-map (kbd "q") #'vundo-quit)
  (define-key vundo-mode-map (kbd "C-g") #'vundo-quit)
  (define-key vundo-mode-map (kbd "RET") #'vundo-confirm))

;; undo and redo between 'buffers'
(use-package winner
  :straight (:type built-in)
  :hook
  (after-init . winner-mode)
  :bind
  (:map winner-mode-map
	("C-M-b" . winner-undo)
	("C-M-f" . winner-redo)))

;; save minibuffer history
(use-package savehist
  :config
  (savehist-mode))

;; save the cursor location
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(use-package which-key
  :hook
  (after-init . which-key-mode))

;; M-w + w/l to copy/cut the word/line
(use-package easy-kill
  :bind
  ([remap kill-ring-save] . easy-kill)
  ([remap mark-sexp] . easy-mark))

;; assign every marked line a cursor
(use-package multiple-cursors
  :init
  (global-unset-key (kbd "M-<down-mouse-1>"))
  :bind
  ("C-M-<down>" . mc/mark-next-like-this)
  ("C-M-<up>" . mc/mark-previous-like-this)
  ("M-<mouse-1>" . mc/add-cursor-on-click))

;; set rectangle selection
(use-package rectangle-mark
  :straight (:type built-in)
  :bind
  ("C-x SPC" . rectangle-mark-mode)
  ;; string rectangle
  ("C-x r t" . string-rectangle)
  ("C-x r k" . kill-rectangle)
  ("C-x r y" . yank-rectangle)
  ("C-x r o" . open-rectangle)
  ("C-x r c" . clear-rectangle)
  ("C-x r d" . delete-rectangle)
  ("C-x r N" . rectangle-number-lines)
  )

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

;; completion strategy
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; rich annotations of minibuffer
(use-package marginalia
  :hook
  (after-init . marginalia-mode))

;; M-up/down to move text
(use-package move-text
  :config
  (move-text-default-bindings))

;; deleting a whitespace character will delete all whitespace
(use-package hungry-delete
  :hook
  (after-init . global-hungry-delete-mode)
  :config
  ;; left one last whitespace
  (setq hungry-delete-join-reluctantly t))

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

;; English spell checker
(use-package jit-spell
  :bind
  ("C-;" . jit-spell-correct-word)
  :hook
  (text-mode . jit-spell-mode))

;; jit-spell uses ispell as backend
(use-package ispell
  :straight (:type built-in)
  :ensure-system-package aspell
  :config
  (setq ispell-program-name "aspell")
  (setq ispell-dictionary "en_GB"))

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

;; bookmark is for quickly jumping to a file/location
(use-package bookmark
  :defer t
  :straight (:type built-in)
  :config
  (bookmark-bmenu-list))

(provide 'editor)
