(require 'package)
(load-file "~/.emacs.d/config/my_variable.el")
(package-initialize)
; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))
; install the missing packages
(dolist (package package-lists)
  (unless (package-installed-p package)
    (package-install package)))
;; install missing themes
(dolist (package theme-lists)
  (unless (package-installed-p package)
    (package-install package)))

(require 'use-package)
(require 'basic-config)
(require 'mode-config)
(require 'org-config)
(require 'tex-config)

;;auto update
(use-package auto-package-update
   :ensure t
   :config
   (setq auto-package-update-delete-old-versions t
         auto-package-update-interval 7)
   (auto-package-update-maybe))

(use-package all-the-icons
  :ensure t
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  )

(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-theme 'icons)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))


(use-package spaceline
  :init
  (require 'spaceline-config)
  :config
  (spaceline-emacs-theme))
 
(use-package yasnippet
  :init
  (add-to-list 'load-path "~/.emacs.d/elpa/yasnippet-20181015.1212")
  :config
  (yas-global-mode 1)
  )

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))


(use-package smex
  :init (smex-initialize)
  :bind (("M-x" . smex)
	 ("M-x" . smex-major-mode-commands)))

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (global-set-key "\C-s" 'swiper)
)


(use-package projectile
  ;;项目文件跳转
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package avy
  ;;快速跳转字符或行
  :config
  (global-set-key (kbd "C-:") 'avy-goto-char)
  (global-set-key (kbd "C-'") 'avy-goto-char-2)
  (global-set-key (kbd "M-g w") 'avy-goto-word-1)
  (global-set-key (kbd "M-g l") 'avy-goto-line))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (all-the-icons-dired neotree doom-themes seoul256-theme dired-sidebar zenburn-theme yasnippet-snippets use-package transient swiper spaceline smex rainbow-delimiters pyim projectile plantuml-mode pdf-tools org-bullets nyan-mode nord-theme markdown-mode macrostep latex-preview-pane git-commit flycheck flucui-themes dracula-theme company avy auctex atom-one-dark-theme 0blayout))))
 
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "dark slate gray")))))
(put 'set-goal-column 'disabled nil)
