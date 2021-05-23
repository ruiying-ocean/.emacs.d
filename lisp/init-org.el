(use-package org
  :defer t
  :config
  (setq org-startup-indented t)
  (setq org-todo-keywords
	'((sequence "TODO" "DOING"  "|" "DONE" "CANCELED")))
  (add-hook 'org-mode-hook 'org-toggle-pretty-entities)
  (add-hook 'org-agenda-finalize-hook #'my-org-agenda-time-grid-spacing)
  (setq org-capture-templates
	'(("t" "Todo" entry (file+headline "~/.emacs.d/org/inbox.org" "Tasks")
	   "* TODO %?\n  %i\n  %a")
	  ("j" "Journal" entry (file+datetree "~/.emacs.d/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")))
  (setq org-default-notes-file "~/.emacs.d/org/inbox.org")
  (setq org-archive-location "~/.emacs.d/org/achives.org::* From %s")
  (setq org-agenda-files (list  "~/.emacs.d/org/agenda.org"))

  ;;src setting
  (setq org-src-fontify-natively t)
  (org-babel-do-load-languages 'org-babel-load-languages
			       '((emacs-lisp . t)
				 (python . t)
				 (R . t)
				 ))
  ;;local keybinding
  :bind
  (:map org-mode-map
	("C-c a" . org-agenda)
	("C-c c" . org-capture)
	("C-c C-r" . org-archive-subtree))
  :hook
  (org-mode . (lambda()
		(variable-pitch-mode 1)
		(visual-line-mode 1)
		(display-line-numbers-mode -1)
		(flyspell-mode 1)
		;;(org-num-mode 1)
		))
  )

;;use org-superstar-mode to replace org-bullets
(use-package org-superstar
  :defer t
  :config
  (setq org-superstar-special-todo-items t)
  :hook
  (org-mode . org-superstar-mode)
  :custom
  (org-ellipsis "⤵"))

;;prettify-symbols-mode setting
(add-hook 'org-mode-hook 'prettify-symbols-mode)
(setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "λ")
				       ("#+END_SRC" . "λ")
				       ("#+begin_src" . "λ")
				       ("#+end_src" . "λ")
				       (">=" . "≥")
				       ("=>" . "⇨")
				       ("[-]" . "❍" )
				       ("[ ]" .  "☐")
				       ("[X]" . "☑" )))
(setq prettify-symbols-unprettify-at-point 'right-edge)


(use-package org-fancy-priorities
  :defer t
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("❗❗❗" "❗❗" "❗")))

;;Image drag-and-drop for org-mode
(use-package org-download
  :defer t
  :config
  (add-hook 'dired-mode-hook 'org-download-enable))

;;(use-package org-super-agenda)

(provide 'init-org)
;;init-org ends here
