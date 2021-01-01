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
  )

(use-package rainbow-mode
  :mode
  "\\.R\\'"
  )

(provide 'init-R)
