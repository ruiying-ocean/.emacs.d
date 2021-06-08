;;This file customizes Emacs color scheme and themes

;;https://github.com/tumashu/cnfonts
;;use cnfonts-edit-profile to configure

;;English font: Iosevka/Inconsolata/Juliamono/Jetbrains Mono/Roboto Mono/Monaco/Fira Code/SF Mono/Operator Mono
;;Chinese font: Wenquanyi Micro Hei Mono/Sarasa UI SC Mono/Noto Sans CJK SC Mono (work perfectly with Iosevka/Inconsolata)
;;Variable-pitch font, ETBembo/New York
;;Unicode: Symbola

(use-package cnfonts
  :defer t
  :if window-system
  :config
  ;;need to add these lists into cnfonts/program.el file
  (setq cnfonts--custom-set-fontnames
	'(("Iosevka" "Inconsolata" "Jetbrains Mono" "Juliamono")
          ("Ubuntu Mono" "WenQuanYi Micro Hei" "Sarasa Mono SC Nerd")))
  (setq cnfonts--custom-set-fontsizes
	'((14   14.0 14.0)
          (15   15.0 15.0)
          (16   16.0 16.0)
	  ))
  :bind
  (("C-=" . cnfonts-increase-fontsize)
   ("C--" . cnfonts-decrease-fontsize))
  )

;;daemon mode font setting
(if (daemonp)
    (progn
      (message "Daemon is forever!")
      (add-hook 'after-make-frame-functions
		(lambda (frame)
		  (with-selected-frame frame
		    (add-to-list 'default-frame-alist '(font . "SF Mono-14"))))))
  (cnfonts-enable))

;;Install themes
(use-package base16-theme :defer t)
(use-package color-theme-sanityinc-tomorrow :defer t)
(use-package gruvbox-theme :defer t)
(use-package tao-theme :defer t)
(use-package humanoid-themes :defer t)
(use-package twilight-bright-theme :defer t)
(use-package ample-theme :defer t) ;;ample flat is a good option for dark theme
(use-package eziam-theme :defer t) ;;almost perfect light theme
(use-package spacemacs-common
  :defer t
  :ensure spacemacs-theme)

;; (use-package doom-themes
;;   :defer t
;;   :config
;;   (load-theme 'doom-ayu-light t)
;;   (load-theme 'doom-nord-light t)
;;   ;;treemacs setting
;;   (setq doom-themes-treemacs-enable-variable-pitch nil)
;;   (setq doom-themes-treemacs-theme "doom-color")
;;   (doom-themes-treemacs-config)  
;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (doom-themes-org-config))

;;the core of this file, use C-c t to change
;;(load-theme 'humanoid-dark t)
(load-theme 'doom-nord-light t)

;;Transprancy setting
(set-frame-parameter (selected-frame) 'alpha '(97 100))
(add-to-list 'default-frame-alist '(alpha 97 100))

(provide 'init-theme)
