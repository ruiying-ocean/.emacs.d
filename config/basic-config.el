;; This file is for operation habits

;----------------------------------------
;Interface setting
;---------------------------------------
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
;(global-tab-line-mode t)
(setq-default cursor-type 'bar)
(add-hook 'after-init-hook 'display-time-mode)
(setq display-time-format "%B %H:%M %a");;时间格式
(setq system-time-locale nil)
;(setq fancy-splash-image "~/.emacs.d/config/logo.jpg")
;----------------------------------
;Editor setting
;----------------------------------
;;Set coding system
(set-default-coding-systems 'utf-8)
(use-package linum
  :init
  (global-linum-mode 1))

(electric-pair-mode t)
(global-hl-line-mode t)
(show-paren-mode t)
(electric-indent-mode t)
;---------------------------------------
;;Font setting
;---------------------------------------
(use-package base16-theme
  :config (load-theme 'base16-nord t))

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

;------------------------------------
; global shortcuts
;-----------------------------------
(global-set-key (kbd "<f3>") 'recentf-open-files)
(global-set-key (kbd "C-x g") 'magit-status)
;;(global-set-key (kbd "C-x m") 'set-mark-command)

(defun open-config-file()
  "A simple customized function from Zilongshanren."
  (interactive)
  (find-file "~/.emacs.d/config/package-configs.el"))
(global-set-key (kbd "<f2>") 'open-config-file)
 (global-set-key (kbd "C-x k") 'kill-this-buffer)
;(global-set-key (kdb "M-;" 'comment-dwim))
;; 
;-------------------------------------
;Other settings
;--------------------------------------
;;No more backup files~
(setq-default make-backup-files nil)

;;open recent files by typing F3
(use-package recentf
  :defer 0.5
  :config
  (recentf-mode t)
  (add-hook 'after-init-hook 'recentf-mode)
  (setq-default recentf-max-saved-items 20
   recentf-exclude '("/tmp/" "/ssh:"))
  )

;;解决emacs在windows下频繁垃圾回收而导致的卡顿问题
(when (eq system-type 'windows-nt)
  (setq gc-cons-threshold (* 512 1024 1024)) ;;设置垃圾回收上限内存，这里是512M
  (setq gc-cons-percentage 0.5) ;;设置垃圾回收比例
  (setq ring-bell-function 'ignore) ;;关闭windows下烦人的警铃声音
  (run-with-idle-timer 5 t #'garbage-collect)
  (setq garbage-collection-messages t)
  )

(setq ring-bell-function 'ignore)

(if (eq system-type 'darwin)
    (setq mac-command-modifier 'meta))

;;tramp mode to cache password
(setq password-cache-expiry nil)

(provide 'basic-config)
;;; basic-config.el ends here
