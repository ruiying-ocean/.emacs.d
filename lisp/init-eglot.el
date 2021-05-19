;; Python setting
(setq python-indent-guess-indent-offset-verbose nil)
(setq python-shell-interpreter "py")
(setq python-shell-interpreter-args "-3.9")
(setq python-indent-offset 4)
;;(add-hook 'python-mode-hook(lambda() (flycheck-mode -1)))

;;Rlang setting
;;Require ESS
(use-package ess
  :config
  (defun R_pipe_operator ()
  "R pipe (%>%) in magrittr"
  (interactive)
  (just-one-space 1)
  (insert "%>%"))
  ;;(reindent-then-newline-and-indent))
  (global-set-key (kbd "M-=") 'ess-cycle-assign)
  (global-set-key (kbd "M-p") 'R_pipe_operator)
  :mode
  ("\\.R\\'" . ess-r-mode)
  ;;otherwise use
  ;;(add-to-list 'auto-mode-alist '("\\.R\\'" . ess-r-mode))
  )

(use-package rainbow-mode
  :after ess
  :hook ess-r-mode
  )

;;eglot can work with tramp-mode, but you should install
;;your server-programs on remote, not local
(use-package eglot
  :config
  ;;============================================
  ;;make sure every command works separately in shell environment. Note R can be tricky in zsh due to the built-in command "r"
  (set 'ad-redefinition-action 'accept)
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) ("clangd"))) ;;install clangd first
  (add-to-list 'eglot-server-programs '(f90-mode . ("fortls"))) ;;pip3 install fortran-language-server
  (add-to-list 'eglot-server-programs '((tex-mode context-mode texinfo-mode bibtex-mode)
                                      . ("digestif"))) ;;luarocks install digestif
  (add-to-list 'eglot-server-programs '(python-mode . ("pylsp"))) ;;pip3 install python-lsp-server
  (add-to-list 'eglot-server-programs '(ess-r-mode . ("R" "--slave" "-e" "languageserver::run()"))) ;;install.packages("languageserver")
  ;;============================================
  :hook
  (python-mode . eglot-ensure)
  (f90-mode . eglot-ensure)
  (ess-r-mode . eglot-ensure)
  :bind
  (:map eglot-mode-map
    ("C-c h" . eldoc)
    ("C-c r" . elgot-rename))
)

(provide 'init-eglot)
;;;init-eglot.el ends here