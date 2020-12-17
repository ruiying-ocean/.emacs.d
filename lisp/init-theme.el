
;---------------------------------------
;;Font and Theme setting
;---------------------------------------
(use-package base16-theme
  :ensure t)

(cond
 ((eq system-type 'windows-nt)
  (load-theme 'base16-dracula t))
 ((eq system-type 'gnu/linux)
  (load-theme 'base16-nord t))
 )

; (use-package doom-themes
;   :ensure t
;   :config
;   ;; Global settings (defaults)
;   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;         doom-themes-enable-italic t) ; if nil, italics is universally disabled
;   (load-theme 'doom-dracula t)

;   ;; Enable flashing mode-line on errors
;   (doom-themes-visual-bell-config)
  
;   ;; Enable custom neotree theme (all-the-icons must be installed!)
;   (doom-themes-neotree-config)
;   ;; or for treemacs users
;   (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
;   (doom-themes-treemacs-config)
  
;   ;; Corrects (and improves) org-mode's native fontification.
;   (doom-themes-org-config))

;;Set Chinese and English font
(set-frame-font "Jetbrains Mono-12")
(dolist (charset '(kana han cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font) charset
                    (font-spec :family "PingFang SC" :size 16)))

;.(use-package cnfonts
;;  :ensure t
;  :config
;  (cnfonts-enable)
;  (setq cnfonts-profiles '("program" "org-mode" "others"))
;  (setq cnfonts--custom-set-fontsizes
;      '((14   15.0 15.0)
;        (16   17.0 17.0)
;        (18   18.0 18.0)
;        (20   21.0 21.0)))
;  :bind
;  (("C-=" . cnfonts-increase-fontsize)
;   ("C--" . cnfonts-decrease-fontsize))
;)

(provide 'init-theme)

