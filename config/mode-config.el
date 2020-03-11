;;global mode
(show-paren-mode 1)
(global-company-mode 1)
(add-hook 'after-init-hook #'global-flycheck-mode)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(setq-default cursor-type 'bar)
(global-hl-line-mode 1)
(set-face-background 'hl-line "light gray")
;(set-face-foreground 'hl-line "sea green")
(recentf-mode 1)
(global-set-key (kbd "<f3>") 'recentf-open-files)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x m") 'set-mark-command)
(nyan-mode t)
(electric-indent-mode 1)
(electric-pair-mode 1)
(display-time-mode 1);;显示时间
(setq display-time-format "%B %H:%M %a");;时间格式
(setq system-time-locale nil)
(setq electric-pair-pairs '(
                            (?\" . ?\")
                            (?\` . ?\`)
                            (?\( . ?\))
                            (?\{ . ?\})
                            ))
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)

(provide 'mode-config)
