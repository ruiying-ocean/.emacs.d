;;recommend to use conda in Windows
(setq python-indent-guess-indent-offset-verbose nil)
(setq python-shell-interpreter "python3")
(setq python-indent-offset 4)
(add-hook 'python-mode-hook
	  (lambda()
	    (flycheck-mode -1)))

(use-package elpy
  :defer 4
  :init
  (elpy-enable)
  :config
 (add-hook 'python-mode-hook 'eldoc-mode)
  (setq python-shell-interpreter "ipython" ;require pip install ipython
      python-shell-interpreter-args "-i --simple-prompt")
  (setq elpy-rpc-python-command "python3")
  (setq elpy-shell-echo-output nil) ;; to solve the ^G^G^G bug
  (setq python-shell-completion-native-enable nil)
  (setq elpy-rpc-backend "jedi")
  (setq python-indent-offset 4
        python-indent 4)
  )

;; (use-package company-jedi
;;   ;;require external installation of jedi and epc in pip
;;   :defer t
;;   :config
;;   (setq jedi:environment-root "jedi");;manually set virtualenv
;;   (setq jedi:server-command (jedi:-env-server-command))
;;   (defun config/enable-jedi ()
;;     (add-to-list 'company-backends 'company-jedi))
;;   (add-hook 'python-mode-hook 'jedi:setup)
;;   (add-hook 'python-mode-hook 'config/enable-jedi)
;;   (setq jedi:complete-on-dot t)
;;   (setq jedi:use-shortcuts t)
;;   (setq company-tooltip-align-annotations t)
;;   (setq company-transformers '(company-sort-by-occurrence))
;;  )

(provide 'init-python)

