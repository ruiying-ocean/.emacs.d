(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(setq-default cursor-type 'bar)
(add-hook 'after-init-hook 'display-time-mode)
(add-hook 'after-init-hook 'display-battery-mode)
(setq display-time-format "%B %H:%M %a");;时间格式
(setq system-time-locale nil)
;;(setq inhibit-startup-screen t)
;;(setq inhibit-startup-message t)

(use-package nyan-mode
  :config
  (nyan-mode t))

(use-package dashboard
    :ensure t
    :diminish dashboard-mode
    :config
    (setq dashboard-banner-logo-title "Welcome back, Rui!")
    (setq dashboard-startup-banner 3)
    (setq dashboard-center-content t)
    (setq dashboard-items '((recents  . 10)
                            (agenda . 5)))
    (dashboard-setup-startup-hook)
    (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))) ;; show Dashboard in frames created with emacsclient -c
    )

(use-package highlight-indent-guides
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

;; (use-package minimap
;;   :custom
;;   (minimap-window-location "right")
;;   (minimap-width-fraction 0.01)
;;   (minimap-minimum-width 3)
;;   :config
;;   (minimap-mode 1)
;;   )

(provide 'init-ui)
