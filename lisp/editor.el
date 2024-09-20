;;Display line number
(global-display-line-numbers-mode t) ;;the linum-mode has been obsolete
(setq display-line-numbers-width 0)


;; overrides certain minor modes and variables to
;; improve the perforamce when open files with long lines
(use-package so-long
  :ensure nil
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


;; auto revert buffer
(use-package autorevert
  :ensure nil
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

;; undo and redo between 'buffers'
(use-package winner
  :ensure nil
  :hook
  (after-init . winner-mode)
  :bind
  (:map winner-mode-map
	("C-M-b" . winner-undo)
	("C-M-f" . winner-redo)))


;; save the cursor location
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

;; set rectangle selection
(use-package rectangle-mark
  :ensure nil
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






(use-package ediff
  :ensure nil
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

;;-----------Dired setting/replacement-------------
(use-package dired
  :ensure nil
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


(provide 'editor)
