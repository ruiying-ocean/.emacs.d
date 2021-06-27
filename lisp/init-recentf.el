(use-package recentf
  :ensure nil
  :config
  (setq-default
   recentf-max-saved-items 30
   recentf-exclude `("/tmp/", (concat package-user-dir "/.*-autoloads\\.el\\'")))
  (global-set-key (kbd "<f3>") 'recentf-open-files) ;;use C-c C-r to call counsel-recentf
  :hook
  (after-init . recentf-mode))

;;use undo-tree-visualize to show history
(use-package undo-tree
  :defer t
  :hook
  (after-init . global-undo-tree-mode)
  :bind
  ("C-c u" . undo-tree-visualize)
  )

(provide 'init-recentf)
;;; init-recentf.el ends here
