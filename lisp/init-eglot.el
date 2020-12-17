(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd")) ;;install clangd first
  (add-to-list 'eglot-server-programs '(f90-mode . ("fortls"))) ;;install fortls through pip first

  (add-hook 'python-mode-hook 'eglot-ensure)
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'java-mode-hook 'eglot-ensure))

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
