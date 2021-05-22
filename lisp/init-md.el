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
  (setq grip-github-user "Leslieying")
  ;;then Creating a personal access token for the command line
  ;;and set the new token to grip-github-password
)

(provide 'init-md)
