; This file is setting of all programming-related packages in use-package framework
(setq use-package-always-ensure t)

(use-package benchmark-init
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;;To specify new version of git on remote machine so I can run magit locally
;;add .ssh/config first
(use-package tramp
  :defer t
  :config
  (add-to-list 'tramp-remote-path "/usr/local/bin/git")
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  ;;tramp mode to cache password
  (setq password-cache-expiry nil)
  )

(use-package counsel-tramp
  :defer t
  :requires tramp
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
;;(use-package ripgrep)
;;(use-package rg)
(use-package ag
  :defer t)
(use-package dumb-jump
  :defer t
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
  :requires projectile
;;an ivy UI for projectile
  :bind
  (:map projectile-mode-map
	  ("C-c p" . projectile-command-map)
    ))

(use-package flycheck
  :defer t
  :hook
  (after-init . global-flycheck-mode)
  :config
  (with-eval-after-load 'flycheck
    (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
  )

(use-package flycheck-inline
  :defer t
  :config
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook #'flycheck-inline-mode)))

(use-package yasnippet
;you may wanna try ivy-yasnippet someday
  :defer 4
  :ensure yasnippet-snippets
  :hook (after-init . yas-global-mode)
  )

(use-package transient
  :defer t)

(use-package magit
  :defer t
  :bind
  ("C-x g" . magit-status)  
  ("C-x c" . magit-checkout))

(use-package exec-path-from-shell
   :if (memq window-system '(mac ns))
   :config
   (exec-path-from-shell-initialize))

(use-package popwin
  :init (require 'popwin)
  :config
  (popwin-mode t))

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

(use-package helpful
  :defer t
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  )

(use-package ace-window
  :defer t
  :config
  (global-set-key (kbd "M-o") 'ace-window))

(provide 'prog-configs)
;;; prog-configs.el ends here
