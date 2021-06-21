;;==============================
;; Python-mode setting
;;==============================
(setq python-shell-interpreter "python3.9")
;; or ipython
;;(setq python-shell-interpreter "ipython"
;;        python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True")

;;python-style indent
(setq python-indent-offset 4)
(setq python-indent-guess-indent-offset-verbose nil) ;;don't complain about the indent anymore

;;debug setting
(setq python-shell-completion-native-enable nil) ;;or pip3 install pyreadline to avoid warning
(setq python-shell-prompt-detect-failure-warning nil)

;;python-mode local keybinding
(with-eval-after-load 'python
  (defun python-run-current-line()
    "a wrapper of python-shell-send-statement"
    (interactive)
    (python-shell-send-statement)
    (forward-line))
  (define-key python-mode-map (kbd "C-<return>") 'python-run-current-line)
  )

;;================================
;;jupyter notebook integration
;;================================
;;[emacs-jupyter] allow you to use org-mode to
;;replace jupyter, while [ein] is a purer jupyter-notebook tool
;;<find other configuration in init-org.el>

;; (use-package jupyter
;;  :defer t)
;;1. Require Emacs with module-support
;;2. Make sure python3 is in your $PATH or alias python -> python3; pip -> python3
;;3. run `pip install ipykernel` `python -m ipykernel install --user`
;;4. This package use zmq which makes Emacs very slow

(use-package ein
  :defer 3)

;; ==============================
;;ESS-R-mode setting (require ESS installed)
;;==============================
;;Lazy load ess-r-mode (ESS doesn't like use-package pretty much)
(unless (package-installed-p 'ess)
  (package-refresh-contents)
  (package-install 'ess))

(use-package ess
  :defer t)

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

;;C-c C-a to turn on csv-align-fields
(use-package csv-mode
  :defer t
  :mode
  "\\.csv\\'"
  "\\.CSV\\'"
  )

;;display color of RGB code
(use-package rainbow-mode
  :defer t
  :after ess
  :hook ess-r-mode
  )

;;==============================
;;Eglot setting, a LSP implementation
;;==============================
;;eglot can work with tramp-mode, but you should install
;;your server-programs on remote, not local
(use-package eglot
  :defer t
  :config
  (add-hook 'eglot-managed-mode-hook (lambda () (flymake-mode -1))) ;;Decouple flymake and eglot
  ;;============================================
  ;;make sure every command works separately in shell environment. Note R can be tricky in zsh due to the built-in command "r"
  (set 'ad-redefinition-action 'accept)
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) ("clangd"))) ;;install clangd first
  (add-to-list 'eglot-server-programs '(f90-mode . ("fortls"))) ;;pip3 install fortran-language-server
  (add-to-list 'eglot-server-programs '((LaTeX-mode tex-mode context-mode texinfo-mode bibtex-mode) ;;use tex-lab or digestif as server
					. ("texlab")))
  (add-to-list 'eglot-server-programs '(python-mode . ("pylsp"))) ;;pip3 install python-lsp-server
  (add-to-list 'eglot-server-programs '(ess-r-mode . ("R" "--slave" "-e" "languageserver::run()"))) ;;install.packages("languageserver")
  ;;============================================
  :hook
  (python-mode . eglot-ensure)
  (f90-mode . eglot-ensure)
  (ess-r-mode . eglot-ensure)
  (LaTeX-mode . eglot-ensure)
  ;;============================================
  ;;local keybindings
  :bind
  (:map eglot-mode-map
	("C-c r" . eglot-rename)
	("C-c h" . eldoc))
  ;;or add follwing lines to :config section
  ;;(define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
  ;;(define-key eglot-mode-map (kbd "C-c h") 'eldoc)
  )

;; MATLAB and Emacs integration:
;; Install
;; cd /path/to/matlab-emacs -> make
;; Homepage: https://sourceforge.net/p/matlab-emacs/src/ci/documentation/tree/
(use-package matlab-mode
  :defer t
  :mode "\\.[mM]\\'"
  )

(provide 'init-eglot)
;;;init-eglot.el ends here
