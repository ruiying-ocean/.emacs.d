;----------------------------------
;Editor setting
;----------------------------------
(set-default-coding-systems 'utf-8)
(electric-pair-mode t)
(global-hl-line-mode t)
(global-display-line-numbers-mode t);;the linum-mode has been obsolete
(setq display-line-numbers-width 0)
(show-paren-mode t)
(setq show-paren-style 'parenthesis)
(electric-indent-mode t)

;------------------------------------
;Other settings
;--------------------------------------
;;No more backup files~
(setq-default make-backup-files nil)
(setq ring-bell-function 'ignore)

;;解决emacs在windows下频繁垃圾回收而导致的卡顿问题
(when (eq system-type 'windows-nt)
  (setq gc-cons-threshold (* 512 1024 1024)) ;;设置垃圾回收上限内存，这里是512M
  (setq gc-cons-percentage 0.5) ;;设置垃圾回收比例
  (run-with-idle-timer 5 t #'garbage-collect)
  (setq garbage-collection-messages t)
  )

;;tramp mode to cache password
(setq password-cache-expiry nil)

(provide 'basic-config)
;;; basic-config.el ends here
