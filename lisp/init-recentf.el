(add-hook 'after-init-hook 'recentf-mode)
(setq-default
 recentf-max-saved-items 30
 recentf-exclude `("/tmp/" "/ssh:" ,(concat package-user-dir "/.*-autoloads\\.el\\'")))
(global-set-key (kbd "<f3>") 'recentf-open-files)

(provide 'init-recentf)
;;; init-recentf.el ends here
