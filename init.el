(require 'package)

(setq my-name "Ying Rui"
      my-email "Ying.Rui@outlook.com")
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")))
(add-to-list 'load-path "~/.emacs.d/config")
(package-initialize)

(require 'use-package)
(require 'package-lists)
(require 'basic-config)
(require 'org-config)
(require 'package-configs.el)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("55c2069e99ea18e4751bd5331b245a2752a808e91e09ccec16eb25dadbe06354" default)))
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
