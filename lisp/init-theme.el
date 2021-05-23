;;https://github.com/tumashu/cnfonts
;;use cnfonts-edit-profile to configure

;;English font: Iosevka/Inconsolata/Juliamono/Jetbrains Mono/Roboto Mono/Monaco/Fira Code/SF Mono/Operator Mono
;;Chinese font: Wenquanyi Micro Hei Mono/Sarasa UI SC Mono/Noto Sans CJK SC Mono (work perfectly with Iosevka/Inconsolata)
;;Variable-pitch font, ETBembo/New York
;;Unicode: Symbola

(use-package cnfonts
  :defer t
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
		    (cnfonts-enable)))))
  (cnfonts-enable))

;;Install themes
(use-package base16-theme :defer t)
(use-package color-theme-sanityinc-tomorrow :defer t)
(use-package gruvbox-theme :defer t)
(use-package sublime-themes :defer t)
(use-package tao-theme :defer t)
(use-package spacemacs-common
  :defer t
  :ensure spacemacs-theme)

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-ayu-light t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;;treemacs setting
  (setq doom-themes-treemacs-enable-variable-pitch nil)
  (setq doom-themes-treemacs-theme "doom-color")
  (doom-themes-treemacs-config)
  
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))


;;Transprancy setting
(set-frame-parameter (selected-frame) 'alpha '(97 100))
(add-to-list 'default-frame-alist '(alpha 97 100))

;;random theme
(setq color-themes '(sanityinc-tomorrow-night base16-zenburn gruvbox-dark-soft spacemacs-dark spolsky doom-one doom-vibrant doom-dark+))
(defun random-color-theme()
  "Want some fresh color? Run me to get some random surprises"
  (interactive)
  (random t)
  (load-theme
   (nth (random (length color-themes)) color-themes)
   t))
(global-set-key (kbd "C-z") 'random-color-theme)
;;(add-hook 'after-init-hook 'random-color-theme)

(provide 'init-theme)

