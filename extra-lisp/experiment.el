
;;; EMACS LSP

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


;;; EAF, Emacs application framework

;; Installation (take care of the dependencies as homepage)
;; git clone --depth=1 -b master https://github.com/manateelazycat/emacs-application-framework.git (require git-lfs)
;; run eaf-install-dependencies

(use-package eaf
  :load-path "~/.emacs.d/emacs-application-framework"
  :init
  (use-package epc :defer t :ensure t)
  (use-package ctable :defer t :ensure t)
  (use-package deferred :defer t :ensure t)
  (use-package s :defer t :ensure t)
  :custom
  (eaf-browser-continue-where-left-off t)
  :config
  (eaf-setq eaf-browser-enable-adblocker "true")
  (eaf-bind-key scroll_up "j" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down "k" eaf-pdf-viewer-keybinding)
  (eaf-bind-key nil "M-q" eaf-browser-keybinding)
  )

;;https://gitlab.com/blak3mill3r/emacs-ludicrous-speed
;;to further improve the startup speed, use this in Linux

(provide 'experiment)
