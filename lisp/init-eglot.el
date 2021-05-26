;; Python setting
(setq python-indent-guess-indent-offset-verbose nil)
(setq python-shell-interpreter "py")
(setq python-shell-interpreter-args "-3.9")
(setq python-indent-offset 4)
;;(add-hook 'python-mode-hook(lambda() (flycheck-mode -1)))

;;R setting (ESS doesn't like use-package pretty much)
;;Require ESS installed
;;Lazy load ess-r-mode
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


(provide 'init-eglot)
;;;init-eglot.el ends here
