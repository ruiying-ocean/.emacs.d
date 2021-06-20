;; This file is setting of all programming-related packages in use-package framework

;; Can be replaced by use-package-report
;; (use-package benchmark-init
;;   :config
;;   ;; To disable collection of benchmark data after init is done.
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))

;;auto-completion system
(use-package company
  :config
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
  :hook
  (after-init . global-company-mode)
  )

(use-package company-org-block
  :defer t
  :custom
  (company-org-block-edit-style 'auto) ;; 'auto, 'prompt, or 'inline
  :hook ((org-mode . (lambda ()
                       (setq-local company-backends '(company-org-block))
                       (company-mode +1))))
  )

;;A match-learning based backend for company
;;May conflict with company-flx-mode/ESS mode
(use-package company-tabnine
  :defer 1
  :after company
  :config
  ;;M-x company-tabnine-install-binary to install binary system
  (if (not (file-directory-p "~/.TabNine/"))
      (company-tabnine-install-binary))
  (add-to-list 'company-backends #'company-tabnine)
  (setq company-idle-delay 0.8)
  (setq company-show-numbers t)
  )

;;A fuzzy matching of company
(use-package company-flx
  :hook
  (company-mode . company-flx-mode)
  )

;;simple and fast sorting and filtering framework for comppany
(use-package company-prescient
  :hook (company-mode . company-prescient-mode)
  :config
  (setq prescient-filter-method '(literal regexp initialism))
  )

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
  (setq company-box-icons-alist 'company-box-icons-images)
  )

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
  (setq password-cache-expiry nil)
  )

(use-package counsel-tramp
  :defer 1
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
(use-package dumb-jump
  :defer 4
  :ensure ag
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-default-project "~/cgenie.muffin")
  (add-to-list 'auto-mode-alist '("\\.config\\'" . shell-script-mode)))

(use-package which-key
  :config
  (which-key-mode t)
  )

(use-package projectile
  :hook
  (after-init . projectile-mode)
  :config
  (setq projectile-completion-system 'ivy)
  :bind
  (:map projectile-mode-map
	("C-c p" . projectile-command-map))
  )

;;an ivy UI for projectile
(use-package counsel-projectile
  :requires projectile
  :bind
  (:map projectile-mode-map
	("C-c p" . projectile-command-map))
  )

;;on-the-fly syntax checker
(use-package flycheck
  :defer t
  :hook
  (prog-mode . flycheck-mode)
  :config
  (with-eval-after-load 'flycheck
    (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
  )

(use-package flycheck-inline
  :defer t
  :hook
  (flycheck-mode . flycheck-inline-mode)
  )

(use-package yasnippet
  :ensure yasnippet-snippets  ;; Collection of snippets
  :hook (after-init . yas-global-mode)
  )

;;manually choose a snippet
(use-package ivy-yasnippet
  :bind
  (("C-c i" . ivy-yasnippet))
  :config
  (setq ivy-yasnippet-expand-keys 'smart))

;;Git + Emacs = boom!
(use-package magit
  :defer t
  :bind
  ("C-x g" . magit-status)  
  ("C-x c" . magit-checkout))

;;a magit prefix help page
(use-package transient
  :defer t)

;;To read proper environment variable in MacOS GUI version
;;To speed up this package, separate configuration into
;;non-interactive (.zshenv) and interactive (.zshrc) part.
;;Find out more in https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (setq exec-path-from-shell-arguments nil) ;;read non-interactive config
  ;;specify some PATH
  (setq exec-path-from-shell-copy-env "/usr/local/bin/python3")
  (exec-path-from-shell-initialize)
  )

(use-package use-package-ensure-system-package
  :defer t
  :after exec-path-from-shell) ;;extend use-package, put after exec-path-from-shell

(use-package popwin
;;  :init (require 'popwin)
  :hook
  (after-init . popwin-mode))

(use-package ivy
  :defer 1
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
  (("C-c b" . counsel-imenu);; something more useful for professional programmer
   ("C-x C-f" . counsel-find-file)
   ("M-x" . counsel-M-x)
   ("M-y" . counsel-yank-pop) ;;something like a clipboard
   ("C-h f" . counsel-describe-function)
   ("C-h v" . counsel-describe-variable)
   ("C-c t" . counsel-load-theme)
   ("C-c j" . counsel-git-grep)
   ("C-c g" . counsel-git) ;;find file in current git directory
   ("C-c l" . counsel-git-log)
   ("C-c r" . counsel-rg) ;;rg find tex
   ("C-c f" . counsel-fzf) ;;fzf find file
   ))

;;sorting and filtering framework for ivy
(use-package ivy-prescient
  :hook (ivy-mode . ivy-prescient-mode)
  :config
  (setq ivy-prescient-sort-commands t
	ivy-prescient-enable-sorting nil
	ivy-prescient-retain-classic-highlighting t)  
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

;;Automatically upgrade packages and allow download from github (too slow)
;; (use-package quelpa
;;   :defer t
;;   :config
;;   (setq quelpa-upgrade-interval 30)
;;   (add-hook #'after-init-hook #'quelpa-upgrade-all-maybe)
;; )

;;quela-use-package (too slow)
;;  (quelpa
;;   '(quelpa-use-package
;;     :fetcher git
;;     :url "https://github.com/quelpa/quelpa-use-package.git"))
;; (require 'quelpa-use-package)

(provide 'init-core)
;;; init-core.el ends here
