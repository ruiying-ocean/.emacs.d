(setq python-shell-interpreter "ipython" ;require pip install ipython
      python-shell-interpreter-args "-i --simple-prompt")

;; (use-package elpy
;;   :init
;;   (elpy-enable)
;;   :config
;;   (add-hook 'python-mode-hook 'eldoc-mode)
;;   (setq elpy-rpc-python-command "python3")
;;   (setq elpy-shell-echo-output nil) ;; to solve the ^G^G^G bug
;;   (setq python-shell-completion-native-enable nil)
;;   (setq elpy-rpc-backend "jedi")
;;   (setq python-indent-offset 4
;;         python-indent 4)
;;   )

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

;;grip mode need to run pip install grip first
(use-package grip-mode
  :bind (:map markdown-mode-command-map
         ("g" . grip-mode)))

(provide 'init-python.el)

