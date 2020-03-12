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

;----------------------------------
;Editor setting
;----------------------------------
;;Set coding system
(set-default-coding-systems 'utf-8)
;;show the line number
(require 'linum)
(global-linum-mode t)
;;show matching pairs of parentheses and other characters
(show-paren-mode 1)
;;make typing parentheses pair
(electric-indent-mode 1)
(electric-pair-mode 1)
(setq electric-pair-pairs '(
                            (?\" . ?\")
                            (?\` . ?\`)
                            (?\( . ?\))
                            (?\{ . ?\})
                            ))
;;highlight current line
(global-hl-line-mode 1)
(set-face-background 'hl-line "light gray") ;(set-face-foreground 'hl-line "sea green")

;---------------------------------------
;;color theme and font setting
;---------------------------------------
;;Configure color theme
(load-theme 'dracula t)
;;Set Chinese and English font
(set-frame-font "fira code-12")
(dolist (charset '(kana han cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font) charset
                    (font-spec :family "更纱黑体 UI SC" :size 21)))

;------------------------------------
; global shortcuts
;-----------------------------------
(global-set-key (kbd "<f3>") 'recentf-open-files)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x m") 'set-mark-command)
(defun open-my-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "<f2>") 'open-my-init-file)
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;-------------------------------------
;Other settings
;--------------------------------------
;;No more backup files~
(setq-default make-backup-files nil)
;;open recent files by typing F3
(recentf-mode 1)
;;解决emacs在windows下频繁垃圾回收而导致的卡顿问题
(when (eq system-type 'windows-nt)
  (setq gc-cons-threshold (* 512 1024 1024)) ;;设置垃圾回收上限内存，这里是512M
  (setq gc-cons-percentage 0.5) ;;设置垃圾回收比例
  (setq ring-bell-function 'ignore) ;;关闭win下烦人的警铃声音
  (run-with-idle-timer 5 t #'garbage-collect)
  (setq garbage-collection-messages t)
  (setenv "HOME" "C:/Users/Leslie_Ying")
  (setenv "PATH" "C:/Users/Leslie_Ying/emacs-26.3-x86_64")
  (setq default-directory "~/")
  )

(provide 'basic-config)
;;; basic-config.el ends here
