;; Start fullscreen (cross-platf)
;;(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)
;; Start maximised (cross-platf)
(add-hook 'window-setup-hook 'toggle-frame-maximized t)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(setq-default cursor-type 'bar)
(add-hook 'after-init-hook 'display-time-mode)
(add-hook 'after-init-hook 'display-battery-mode)
(setq display-time-format "%B %H:%M %a");;时间格式
(setq system-time-locale nil)
;;(setq inhibit-startup-screen t)
;;(setq inhibit-startup-message t)
(global-hl-line-mode t)
(global-display-line-numbers-mode t);;the linum-mode has been obsolete
(setq display-line-numbers-width 0)
(show-paren-mode t)
(setq show-paren-style 'mixed)
;;(electric-indent-mode t)

;;more see the doc https://github.com/Fuco1/smartparens/wiki/Working-with-expressions
(use-package smartparens
  :init (smartparens-global-mode t)
  :bind
  (:map smartparens-mode-map
	("C-M-f" . sp-forward-sexp)
	("C-M-b" . sp-backward-sexp)
	("C-M-d" . sp-down-sexp);;down one level
	("C-M-u" . sp-up-sexp));;up one level
  :config  
  (sp-pair "\{" "\}") ;; latex literal brackets (included by default)
  (sp-pair "<#" "#>")
  (sp-pair "$" "$")   ;; latex inline math mode. Pairs can have same opening and closing string)
  (sp-local-pair 'LaTeX-mode "\\left(" "\\right)" :insert "C-b l" :trigger "\\l(")
  (sp-pair "'" nil :actions :rem)
  )

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  )

(use-package nyan-mode
  :config
  (nyan-mode t))

;; (use-package spaceline
;;  :init
;;  (require 'spaceline-config)
;;  :config
;;  (spaceline-emacs-theme))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(use-package dashboard
    :diminish dashboard-mode
    :config
    (setq dashboard-banner-logo-title "Welcome back, Rui!")
    (setq dashboard-startup-banner 3)
    (setq dashboard-center-content t)
    (setq dashboard-items '((recents  . 10)
                            (agenda . 5)))
    (dashboard-setup-startup-hook)
    (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))) ;; show Dashboard in frames created with emacsclient -c
    )

(use-package highlight-indent-guides
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
;  (set-face-background 'highlight-indent-guides-character-face "dimgrey")
  :custom
  (highlight-indent-guides-method 'character)
  )

(use-package highlight-symbol
;; An alternative package is highlight-thing
  :bind
  ("C-<f9>" . highlight-symbol)
  ("<f9>" . highlight-symbol-next)
  ("S-<f9>" . highlight-symbol-prev)
  ("M-<f9>" . highlight-symbol-query-replace)
  )

(use-package minimap
  :config
  (minimap-mode -1)
  :custom
  (minimap-window-location 'right)
  (minimap-width-fraction 0.05)
  (minimap-minimum-width 15)
  )

(use-package neotree
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (global-set-key [f8] 'neotree-toggle)
  )

(use-package awesome-tab
  ;;not currently available on melpa
  :load-path "~/.emacs.d/config/awesome-tab.el"
  :config
  (awesome-tab-mode t)
  (setq awesome-tab-show-tab-index t)
  (setq awesome-tab-height 100)
  :bind
  ("C-x i" . awesome-tab-ace-jump) ;;or TAB
  )  

(use-package visual-fill-column
  :config
  (add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
  :custom
  (visual-fill-column-center-text t)
  (visual-fill-column-width 100)
  )

(provide 'init-ui)
