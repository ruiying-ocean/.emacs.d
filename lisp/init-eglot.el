(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd")) ;;install clangd first
  (add-to-list 'eglot-server-programs '(f90-mode . ("fortls"))) ;;pip3 install fortran-language-server
  (add-to-list 'eglot-server-programs '((tex-mode context-mode texinfo-mode bibtex-mode)
                                      . ("texlab"))) ;;install texlab and push to $PATH
  :hook
  (python-mode . eglot-ensure) ;;pip3 install python-language-server
  (c-mode . eglot-ensure)
  (java-mode . eglot-ensure)
;;  (ess-r-mode . eglot-ensure);;install.packages("languageserver")
  )

;; install quick-run if you need
;; (use-package quickrun
;;   :config
;;   (quickrun-add-command "c++/c1z"
;; 			'((:command . "g++")
;; 			  (:exec    . ("%c -std=c++1z %o -o %e %s"
;; 				       "%e %a"))
;; 			  (:remove  . ("%e")))
;; 			:default "c++"))

(provide 'init-eglot)
;;;init-eglot.el ends here
