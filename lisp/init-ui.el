(when (eq system-type 'darwin)
    (progn (add-hook 'window-setup-hook 'toggle-frame-maximized t)
	    (setq dired-use-ls-dired nil))) ;;to avoid error "ls does not support --dired" on MacOS
(add-hook 'after-init-hook 'display-time-mode)
(add-hook 'after-init-hook 'display-battery-mode)
(setq display-time-default-load-average nil)
(setq display-time-format "%B %d %H:%M %p")
(setq system-time-locale nil)
(setq-default cursor-type 'bar)
(blink-cursor-mode 0)
(global-hl-line-mode t)
(global-display-line-numbers-mode t);;the linum-mode has been obsolete
(setq display-line-numbers-width 0)
(show-paren-mode t)
(setq show-paren-style 'mixed)
;;(electric-indent-mode t)
(setq frame-inhibit-implied-resize nil)

;;more see the doc https://github.com/Fuco1/smartparens/wiki/Working-with-expressions
(use-package smartparens
  :defer t
  :hook
  (after-init . smartparens-global-mode)
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
  :defer t
  :hook
  (prog-mode . rainbow-delimiters-mode)
  )

;; (use-package spaceline
;;  :init
;;  (require 'spaceline-config)
;;  :config
;;  (spaceline-emacs-theme))

(use-package doom-modeline
  ;;right fringe cut-off issue should relate to font size
  ;;Use cnfont-decrease-size or see more methods in 
  ;;https://github.com/hlissner/doom-emacs/blob/develop/modules/ui/modeline/README.org#the-right-side-of-the-modeline-is-cut-off
  :defer t
  :after all-the-icons
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-window-width-limit fill-column)
  (setq doom-modeline-icon (display-graphic-p))
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-enable-word-count nil)
  (setq all-the-icons-scale-factor 1.0)
  )

(use-package nyan-mode
  :defer t
  :hook
  (doom-modeline-mode . nyan-mode))

(use-package dashboard
  :diminish dashboard-mode
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Happiness is everything - Rui")
;;    (setq dashboard-startup-banner 3)
  (setq dashboard-startup-banner "~/.emacs.d/fancy-splash/lady.png")
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents  . 3)))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))) ;; show Dashboard in frames created with emacsclient -c
  (setq dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)
  )

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
  :config
  (minimap-mode -1)
  :custom
  (minimap-window-location 'right)
  (minimap-width-fraction 0.05)
  (minimap-minimum-width 15)
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
  (treemacs-load-theme "all-the-icons"))

(use-package all-the-icons-dired
  :defer t
  ;need to run all-the-icons-install-fonts first to avoid grabled icon
  :requires all-the-icons
  :hook
  (dired-mode . all-the-icons-dired-mode))

(use-package ivy-rich
  :defer t
  :after ivy
  :config
  (ivy-rich-mode t)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package all-the-icons-ivy-rich
  :defer t
  :after ivy-rich
  :init
  (all-the-icons-ivy-rich-mode t)
  :config
  (setq all-the-icons-ivy-rich-icon-size 1.0)
  (setq inhibit-compacting-font-caches t)
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
  (centaur-tabs-mode t)
  (setq centaur-tabs-set-bar 'over)
  (setq centaur-tabs-height 32)
  (setq centaur-tabs-gray-out-icons 'buffer)
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-close-button "x")
  (setq centaur-tabs-set-modified-marker t)
  (setq centaur-tabs-modified-marker "*")
  :bind
  ("C-c j" . centaur-tabs-backward)
  ("C-c k" . centaur-tabs-forward)
  :hook
  (dired-mode . centaur-tabs-local-mode)
)

(use-package visual-fill-column
  :defer t
  :hook
  (visual-line-mode . visual-fill-column-mode)
  :custom
  (visual-fill-column-center-text t)
  (visual-fill-column-width 100)
  )

(provide 'init-ui)
