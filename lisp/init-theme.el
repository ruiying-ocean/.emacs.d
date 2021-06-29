;;This file customizes Emacs font, color scheme and themes

;;English font: Iosevka/Inconsolata/Juliamono/Jetbrains Mono/Roboto Mono/Monaco/Fira Code/SF Mono/Operator Mono
;;Chinese font: Wenquanyi Micro Hei Mono/Sarasa UI SC Mono/Noto Sans CJK SC Mono (work perfectly with Iosevka/Inconsolata)
;;Variable-pitch font, ETBembo/New York
;;Unicode: Symbola

(defun init-font()
   (interactive)
   "Set English and CJK font for Emacs"
  ;; English font
  (if (display-graphic-p)
      (progn
	;; English font
        (set-face-attribute 'default nil :font (format "%s:pixelsize=%d" "Iosevka" 16))
        ;; CJK font
        (dolist (charset '(kana han symbol cjk-misc bopomofo))
          (set-fontset-font (frame-parameter nil 'font)
                            charset
                            (font-spec :family "Noto Sans Mono CJK SC"))))
    ))

;; Use emacs daemon, put following lines to shell config file
;; alias ed="emacs --daemon"
;; alias ec="emacsclient -c"
;; alias eq="emacsclient -e '(save-buffers-kill-emacs)'"
(if (daemonp)
    (add-hook 'after-make-frame-functions
	      (lambda (frame)
		(with-selected-frame frame (init-font))))
  (add-hook 'after-init-hook 'init-font)
  )

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

(use-package doom-themes
  :defer t
  ;; :config
  ;; ;;treemacs setting
  ;; (setq doom-themes-treemacs-enable-variable-pitch nil)
  ;; (setq doom-themes-treemacs-theme "doom-color")
  ;; (doom-themes-treemacs-config)  
  ;; ;; Corrects (and improves) org-mode's native fontification.
  ;; (doom-themes-org-config)
  )

;;the core of this file, use C-c t to change
;;(load-theme 'humanoid-dark t)
;;(load-theme 'doom-dark+ t)
(load-theme 'doom-one t)
;;(load-theme 'doom-vibrant t)

;;Transprancy setting
(set-frame-parameter (selected-frame) 'alpha '(97 100))
(add-to-list 'default-frame-alist '(alpha 97 100))

(provide 'init-theme)
