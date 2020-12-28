;----------------------------------
;Editor setting
;----------------------------------
(setq use-package-always-ensure t)
(fset 'yes-or-no-p 'y-or-n-p)
(progn
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8))

(use-package benchmark-init
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;------------------------------------
;Other settings
;--------------------------------------
;;No more backup files~
(setq-default make-backup-files nil)
(setq ring-bell-function 'ignore)

;;tramp mode to cache password
(setq password-cache-expiry nil)

(provide 'basic-config)
;;; basic-config.el ends here
