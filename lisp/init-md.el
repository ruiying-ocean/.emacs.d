(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :hook
  (markdown-mode . (lambda()
		     (display-line-numbers-mode -1)
		     (visual-line-mode 1)))
 )

;;pip install grip first
(use-package grip-mode
  :bind (:map markdown-mode-command-map
              ("g" . grip-mode))
  :hook (markdown-mode . grip-mode)
  :config
  ;;create your personal access token, then config
  ;;your github username and token in "~/.authinfo.gpg"
  ;;DO NOT input your password here!
  (require 'auth-source)
  (let ((credential (auth-source-user-and-password "api.github.com")))
    (setq grip-github-user (car credential)
          grip-github-password (cadr credential)))  
)

;;add table of content for md/org
(use-package toc-org
  :hook
  (markdown-mode . toc-org-mode)
  (org-mode . toc-org-mode)
  :config
  (global-set-key (kbd "\C-c\C-o") 'toc-org-markdown-follow-thing-at-point)
  )




(provide 'init-md)
