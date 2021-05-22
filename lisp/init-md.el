(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :hook (markdown-mode . visual-line-mode)
 )

(add-hook 'markdown-mode-hook
          (lambda ()           
            (visual-line-mode 1)
	    (display-line-numbers-mode -1)
	    ))

;;pip install grip first
(use-package grip-mode
  :bind (:map markdown-mode-command-map
              ("g" . grip-mode))
  :hook(markdown-mode . grip-mode)
  :config
  ;;create your personal access token, then config
  ;;your github username and token in "~/.authinfo.gpg"
  ;;DO NOT input your password here!
  (require 'auth-source)
  (let ((credential (auth-source-user-and-password "api.github.com")))
    (setq grip-github-user (car credential)
          grip-github-password (cadr credential)))  
)

(provide 'init-md)
