;;This file is for customizing Emacs appearance

;;If you are running Emacs in MacOS, then I recommend you using
;;Emacs Mac Port <--with-no-title-bars> which improves GUI performance a lot
(when (eq system-type 'darwin)
  (progn
    (setq dired-use-ls-dired nil);;to avoid error "ls does not support --dired" in MacOS
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)) ;;same title bar color
    (add-to-list 'default-frame-alist '(ns-appearance . light))))

;;Auto-max the frame in non-linux system
;;(when (not (eq system-type 'gnu/linux))
(add-hook 'window-setup-hook 'toggle-frame-maximized t)

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
  (after-init . mood-line-mode)
  )

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

(use-package dashboard
  :if (< (length command-line-args) 2)
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-set-init-info t)
  (setq dashboard-banner-logo-title "Happiness is everything - Rui")
  ;;    (setq dashboard-startup-banner 3)
  (setq dashboard-startup-banner "~/.emacs.d/fancy-splash/world.png")
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents  . 3))) ;;add org-agenda could slow start-up speed
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
  (setq centaur-tabs-height 22)
  ;;(setq centaur-tabs-label-fixed-length 10) ;;fixed length
  (centaur-tabs-change-fonts "JetBrains Mono" 130)
  :bind
  ("M-<left>" . centaur-tabs-backward)
  ("M-<right>" . centaur-tabs-forward)
  :hook
  (dired-mode . centaur-tabs-local-mode)
  (dashboard-mode . centaur-tabs-mode)
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
  :load-path "extra/"
  )

;; (use-package focus
;;   :defer t
;;   :hook
;;   (text-mode . focus-mode)
;;   :config
;;   (add-to-list 'focus-mode-to-thing '((text-mode . sentence)
;; 				      (prog-mode . defun)
;; 				      (latex-mode . paragraph))))

(provide 'init-ui)
;;;init-ui.el ends here
