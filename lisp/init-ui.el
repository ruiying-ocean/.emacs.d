(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
;;(global-tab-line-mode t)
(setq-default cursor-type 'bar)
(add-hook 'after-init-hook 'display-time-mode)
(setq display-time-format "%B %H:%M %a");;时间格式
(setq system-time-locale nil)
;; (setq fancy-splash-image "~/.emacs.d/config/logo.jpg")

(provide 'init-ui)
