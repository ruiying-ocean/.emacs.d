;; This file is for operation habits

;----------------------------------------
;Interface setting
;---------------------------------------
(tool-bar-mode 0)
(scroll-bar-mode 0)
(setq-default cursor-type 'bar)
(display-time-mode 1);;显示时间
(setq display-time-format "%B %H:%M %a");;时间格式
(setq system-time-locale nil)
;;Max the window as startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;(setq fancy-splash-image "~/.emacs.d/config/logo.jpg")
;----------------------------------
;Editor setting
;----------------------------------
;;Set coding system
(set-default-coding-systems 'utf-8)
(use-package linum
  :init (global-linum-mode t)
  )
(electric-pair-mode t)
(global-hl-line-mode t)
(show-paren-mode t)
(electric-indent-mode t)
;---------------------------------------
;;Font setting
;---------------------------------------
(use-package base16-theme
  :config
  (load-theme 'base16-dracula t))
;;Set Chinese and English font
(set-frame-font "fira code-11")
(dolist (charset '(kana han cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font) charset
                    (font-spec :family "更纱黑体 UI SC" :size 22)))

;------------------------------------
; global shortcuts
;-----------------------------------
(global-set-key (kbd "<f3>") 'recentf-open-files)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x m") 'set-mark-command)

(defun open-config-file()
  "A simple customized function from Zilongshanren."
  (interactive)
  (find-file "~/.emacs.d/config/package-configs.el"))
(global-set-key (kbd "<f2>") 'open-config-file)
(global-set-key (kbd "C-x k") 'kill-this-buffer)

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
  (setenv "HOME" "C:/Users/Leslie_Ying/")
  (setenv "PATH" "C:/Users/Leslie_Ying/emacs-26.3-x86_64")
  )

(provide 'basic-config)
;;; basic-config.el ends here
