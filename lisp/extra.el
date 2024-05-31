;; (use-package kind-icon
;;   :after corfu
;;   :custom
;;   (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
;;   :config
;;   (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; (use-package nerd-icons
;;   :custom
;;   (nerd-icons-font-family myfont)
;;   (nerd-icons-scale-factor 1.5))

;; (use-package nerd-icons-dired
;;   :if window-system
;;   :hook
;;   (dired-mode . nerd-icons-dired-mode))

;; (use-package nerd-icons-completion
;;   :config
;;   (nerd-icons-completion-mode))

;; (use-package nerd-icons-ibuffer
;;   :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
;;   :config
;;   (setq nerd-icons-ibuffer-icon t))

;; ;; highlight cursor when scroll window
;; (use-package beacon
;;   :straight (:host github :repo "Malabarba/beacon")
;;   :hook
;;   (after-init . beacon-mode))

;; ;; Change cursor color dynamically at cursor or pointer
;; (use-package smart-cursor-color
;;   :hook
;;   (after-init . smart-cursor-color-mode))

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

(use-package easy-hugo
  :init
  (setq easy-hugo-postdir "content/posts")
  (setq easy-hugo-basedir "~/blog/")
  :config
  (setq easy-hugo-default-ext ".org")
  (setq easy-hugo-org-header nil)
  )

(use-package rime
    :custom
    (default-input-method "rime")
    ;; Path to the Emacs dynamic module
    (rime-emacs-module-header-root "/opt/homebrew/Cellar/emacs-mac/emacs-29.1-mac-10.0/include")
    ;; Path to the librime library
    (rime-librime-root "~/.emacs.d/librime/dist")
    ;; Path to the rime configuration file
    (rime-user-data-dir "~/Library/rime")
    ;; Style of showing candidate
    (rime-show-candidate 'posframe))

(provide 'extra)
