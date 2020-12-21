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
  :ensure t
  :bind (:map markdown-mode-command-map
         ("g" . grip-mode)))

(provide 'init-md)
