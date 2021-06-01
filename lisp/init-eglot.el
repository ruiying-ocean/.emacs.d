;;==============================
;; Python-mode setting
;;==============================
(setq python-indent-guess-indent-offset-verbose nil)
(setq python-shell-interpreter "python3")
;;or If you use jupyter-console
;;(setq python-shell-interpreter "jupyter-console")
;;(setq python-shell-interpreter-args "--simple-prompt")

;;python-style indent
(setq python-indent-offset 4)

;;eglot doesn't like flycheck
;;(add-hook 'python-mode-hook (lambda() (flycheck-mode -1)))

;;debug setting
(setq python-shell-completion-native-enable nil) ;;or pip3 install pyreadline to avoid warning
(setq python-shell-prompt-detect-failure-warning nil)


;;================================
;;jupyter notebook integration
;;================================
;;[emacs-jupyter] allow you to use org-mode to
;;replace jupyter, while [ein] is a purer jupyter-notebook tool
;;<find other configuration in init-org.el>

(use-package jupyter
  :defer t)
;;1. Require Emacs with module-support
;;2. Make sure python3 is in your $PATH or alias python -> python3; pip -> python3
;;3. run `pip install ipykernel` `python -m ipykernel install --user`

;;(use-package ein)

;; ==============================
;;ESS-R-mode setting (require ESS installed)
;;==============================
;;Lazy load ess-r-mode (ESS doesn't like use-package pretty much)
(use-package ess) ;;install emacs-package: ess
(add-to-list 'auto-mode-alist '("\\.R\\'" . ess-r-mode))
(with-eval-after-load 'ess-r-mode
  (defun ess_insert_pipe()
    "R pipe (%>%) in magrittr package"
    (interactive)
    (just-one-space 1)
    (insert "%>%")
    ;;(reindent-then-newline-and-indent)
    )
  (define-key ess-r-mode-map (kbd "M--") 'ess-insert-assign)
  (define-key inferior-ess-r-mode-map (kbd "M--") 'ess-insert-assign)
  (define-key ess-r-mode-map (kbd "M-p") 'ess_insert_pipe)
  (define-key inferior-ess-r-mode-map (kbd "M-p") 'ess_insert_pipe)			 
  )

;;display color of RGB code
(use-package rainbow-mode
  :after ess
  :hook ess-r-mode
  )

;;==============================
;;Eglot setting, a LSP implementation
;;==============================
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
