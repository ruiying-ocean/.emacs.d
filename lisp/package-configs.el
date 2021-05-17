; This file is setting of all packages in use-package framework


;;To specify new version of git on remote machine so I can run magit locally

(use-package tramp
  :config
  (add-to-list 'tramp-remote-path "/usr/local/bin/git")
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  )

(use-package counsel-tramp  
  :config
  (setq tramp-default-method "scp")
  (add-hook 'counsel-tramp-pre-command-hook '(lambda () (global-aggressive-indent-mode 0)
            (projectile-mode 0)
            (editorconfig-mode 0)))
  (add-hook 'counsel-tramp-quit-hook '(lambda () (global-aggressive-indent-mode 1)
            (projectile-mode 1)
            (editorconfig-mode 1)))
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
  :requires ag
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-default-project "~/cgenie.muffin")
  (add-to-list 'auto-mode-alist '("\\.config\\'" . shell-script-mode)))

(use-package which-key
  :config
  (which-key-mode t))

(use-package projectile
  :defer 2
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (projectile-mode +1)
  :bind
  (:map projectile-mode-map
	("C-c p" . projectile-command-map)))

(use-package counsel-projectile
;;an ivy UI for projectile
  :bind
  (:map projectile-mode-map
	  ("C-c p" . projectile-command-map)
    ))

(use-package flycheck
;  :init (global-flycheck-mode)
  :hook
  (after-init . global-flycheck-mode)
  :config
 (with-eval-after-load 'flycheck
   (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
  )

(use-package flycheck-inline
  :config
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook #'flycheck-inline-mode)))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  )

(use-package yasnippet
;you may wanna try ivy-yasnippet someday
  :defer 4
  :ensure yasnippet-snippets
  :hook (after-init . yas-global-mode)
  )

(use-package transient
  :defer t)

(use-package magit
  :bind (("C-x g" . magit-status)))

(use-package exec-path-from-shell
   :if (memq window-system '(mac ns))
   :ensure t
   :config
   (exec-path-from-shell-initialize))

(use-package popwin
  :init (require 'popwin)
  :config
  (popwin-mode t))

(use-package all-the-icons-dired
  ;need to run all-the-icons-install-fonts first to avoid grabled icon
  :requires all-the-icons  
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

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

(use-package smex
  :init (smex-initialize)
  :bind (("M-x" . smex)
	 ("M-x" . smex-major-mode-commands)))

(use-package ivy
  :hook
  (after-init . ivy-mode)
  :config
  ;(ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (global-set-key "\C-s" 'swiper)
)

(use-package flyspell-correct-ivy
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

;; (use-package flyspell-correct-popup
;;   :after flyspell-correct)

(use-package ivy-rich
  :after ivy
  :config
  (ivy-rich-mode t)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package all-the-icons-ivy-rich
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

(use-package counsel
  :after ivy
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-h f") 'counsel-describe-function)
  (global-set-key (kbd "C-h v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(use-package avy
  ;;快速跳转字符或行
  :defer t
  :config
  (global-set-key (kbd "C-:") 'avy-goto-char)
  (global-set-key (kbd "C-'") 'avy-goto-char-2)
  (global-set-key (kbd "M-g w") 'avy-goto-word-1)
  (global-set-key (kbd "M-g l") 'avy-goto-line))

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
  (minimap-mode nil)
  :custom
  (minimap-window-location 'right)
  (minimap-width-fraction 0.05)
  (minimap-minimum-width 15)
  )

(use-package helpful
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  )

(use-package ace-window
  :config
  (global-set-key (kbd "M-o") 'ace-window))

(use-package visual-fill-column
  :config
  (add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
  :custom
  (visual-fill-column-center-text t)
  (visual-fill-column-width 100)
  )

(provide 'package-configs)
;;; package-configs.el ends here
