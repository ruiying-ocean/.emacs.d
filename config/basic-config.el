;;Basic Configuration
;;启动时最大化窗口
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;shortcut to open init.el
(defun open-my-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "<f2>") 'open-my-init-file)
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;;解决emacs在windows下频繁垃圾回收而导致的卡顿问题
(when (eq system-type 'windows-nt) ;;当系统是win时
  (setq gc-cons-threshold (* 512 1024 1024)) ;;设置垃圾回收上限内存，这里是512M
  (setq gc-cons-percentage 0.5) ;;设置垃圾回收比例
  (setq ring-bell-function 'ignore) ;;关闭win下烦人的警铃声音
  (run-with-idle-timer 5 t #'garbage-collect)
  (setq garbage-collection-messages t)
  (setenv "HOME" "C:/Users/Leslie_Ying")
  (setenv "PATH" "C:/Users/Leslie_Ying/emacs-26.3-x86_64")
  (setq default-directory "~/")
  )

;;设置中文和英文字体
(set-frame-font "fira code-12")
(dolist (charset '(kana han cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font) charset
                    (font-spec :family "更纱黑体 UI SC" :size 21)))
;;设置编码
(set-default-coding-systems 'utf-8)

;;configure my theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'atom-one-dark-theme)


;; show the line number
(require 'linum)
(global-linum-mode t)

;;No more backup files~
(setq-default make-backup-files nil)

(provide 'basic-config)
