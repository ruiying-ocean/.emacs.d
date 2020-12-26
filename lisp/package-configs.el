; This file is setting of all packages in use-package framework
(setq use-package-always-ensure t)

(use-package which-key
  :config
  (which-key-mode))

(use-package projectile
  :defer 3
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (projectile-mode +1)
  :bind
  (:map projectile-mode-map
	("C-c p" . projectile-command-map)))

(use-package flycheck
  :init (global-flycheck-mode))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  )

(use-package yasnippet
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

;(use-package spaceline
;  :init
;  (require 'spaceline-config)
;  :config
;  (spaceline-emacs-theme))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

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
  :config
  (ivy-mode 1)
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
  :config
  (ivy-rich-mode t)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

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
  :defer 2
  :config
  (global-set-key (kbd "C-:") 'avy-goto-char)
  (global-set-key (kbd "C-'") 'avy-goto-char-2)
  (global-set-key (kbd "M-g w") 'avy-goto-word-1)
  (global-set-key (kbd "M-g l") 'avy-goto-line))

; (use-package ess-r-mode
;   :ensure ess
;   :config
;   (defun then_R_operator ()
;   "R - %>% operator or 'then' pipe operator"
;   (interactive)
;   (just-one-space 1)
;   (insert "%>%")
;   (reindent-then-newline-and-indent))
;   :bind
;   (:map ess-mode-map
;         ("M-=" . ess-cycle-assign)
;         ("M-p" . then_R_operator))
;   (:map inferior-ess-mode-map
;         ("M-=" . ess-cycle-assign)
; 	("M-p" . then_R_operator))
;   )

(provide 'package-configs)
;;; package-configs.el ends here
