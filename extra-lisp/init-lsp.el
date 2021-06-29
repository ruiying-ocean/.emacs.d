(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-l")
  :hook
  ((python-mode . lsp-deferred)
   (ess-r-mode . lsp-deferred)
   (f90-mode . lsp-deferred)
   (LaTeX-mode . lsp-deferred)
   (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-idle-delay 0.500))

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

;;debug
(use-package dap-mode)

(provide 'init-lsp)
