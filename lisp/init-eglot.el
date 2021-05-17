(use-package python-mode
  :config
  (setq python-indent-guess-indent-offset-verbose nil)
  (setq python-shell-interpreter "python3")
  (setq python-indent-offset 4)
)

(use-package ess-r-mode
  :ensure ess
  :config
  (defun then_R_operator ()
  "R - %>% operator or 'then' pipe operator"
  (interactive)
  (just-one-space 1)
  (insert "%>%")
  (reindent-then-newline-and-indent))
  :bind
  (:map ess-mode-map
        ("M-=" . ess-cycle-assign)
        ("M-p" . then_R_operator))
  (:map inferior-ess-mode-map
        ("M-=" . ess-cycle-assign)
	("M-p" . then_R_operator))
  :mode
  "\\.R\\'"
  "\\.r\\'"
  )

(use-package rainbow-mode
  :mode
  "\\.R\\'"
  )

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd")) ;;install clangd first
  (add-to-list 'eglot-server-programs '(f90-mode . ("fortls"))) ;;pip3 install fortran-language-server
  (add-to-list 'eglot-server-programs '((tex-mode context-mode texinfo-mode bibtex-mode)
                                      . ("digestif"))) ;;install digestif and export to $PATH
  (add-to-list 'eglot-server-program '(python-mode . "pyls")) ;;pip3 install python-lsp-server
  :hook
  (python-mode . eglot-ensure) 
  (c-mode . eglot-ensure)
  (f90-mode . eglot-ensure)
  (ess-r-mode . eglot-ensure);;install.packages("languageserver")
)

(provide 'init-eglot)
;;;init-eglot.el ends here
