(use-package lsp-mode
  :config
  (setq lsp-completion-provider :capf)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq gc-cons-threshold (* 125 1024 1024))
  (setq lsp-idle-delay 0.500)
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp-deferred)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :commands lsp-ui-mode)

;; optionally if you want to use debugger
;;(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; (use-package lsp-treemacs
;;   :commands lsp-treemacs-errors-list)

;; (use-package lsp-ivy
;;   :commands lsp-ivy-workspace-symbol)


;; C/C++/Objective-C support
;; (use-package ccls
;;   :defines projectile-project-root-files-top-down-recurring
;;   :hook ((c-mode c++-mode objc-mode cuda-mode) . (lambda () (require 'ccls)))
;;   :config
;;   (with-eval-after-load 'projectile
;;     (setq projectile-project-root-files-top-down-recurring
;;           (append '("compile_commands.json" ".ccls")
;;                   projectile-project-root-files-top-down-recurring))))

(provide 'init-lsp)
;;; init-lsp ends here
