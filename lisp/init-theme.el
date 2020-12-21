
;---------------------------------------
;;Font and Theme setting
;---------------------------------------
(use-package base16-theme
  :ensure t)

(cond
 ((eq system-type 'windows-nt)
  (load-theme 'spacemacs-dark t))
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

(defun s-font()
  (interactive)
  ;; font config for org table showing.
  (set-frame-font "Jetbrains Mono-12")
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset
                      (font-spec :family "Sarasa UI SC")))
  ;; tune rescale so that Chinese character width = 2 * English character width
  (setq face-font-rescale-alist '(("Jetbrains Mono" . 1.0) ("Sarasa UI SC" . 1.0))))

(add-to-list 'after-make-frame-functions
             (lambda (new-frame)
               (select-frame new-frame)
               (if (display-graphic-p)
                   (s-font))))
(if (display-graphic-p)
    (s-font))

;; (use-package cnfonts
;;   :config
;;   (cnfonts-enable)
;;   (setq cnfonts-profiles '("program" "others")
;;   (setq cnfonts--custom-set-fontnames
;;       '(("PragmataPro" "Ubuntu Mono" "DejaVu Sans Mono")
;;         ("文泉驿等宽微米黑" "Ubuntu Mono" "隶书" "新宋体")
;;         ("HanaMinB" "SimSun-ExtB" "MingLiU-ExtB")))
;;   (setq cnfonts--custom-set-fontsizes
;; 	'((9    9.0  9.5 )
;;           (10   11.0 11.0)
;;           (11.5 12.5 12.5)
;;           (12.5 13.5 13.5)
;;           (14   15.0 15.0)
;;           (16   17.0 17.0)
;;           (18   18.0 18.0)
;;           (20   21.0 21.0)
;;           (22   23.0 23.0)
;;           (24   25.5 25.5)
;;           (26   27.0 27.0)
;;           (28   29.0 29.0)
;;           (30   32.0 32.0)
;;           (32   33.0 33.0)))
;;   :bind
;;   (("C-=" . cnfonts-increase-fontsize)
;;    ("C--" . cnfonts-decrease-fontsize))
;;   ))

(provide 'init-theme)

