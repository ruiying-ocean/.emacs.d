;;read the documentation to find how to compile and pdf-tools first
(use-package pdf-tools
  :defer t
  :commands (pdf-view-mode pdf-tools-install)
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query)
  (define-pdf-cache-function pagelables)
  :hook
  (pdf-view-mode-hook . (lambda () (display-line-numbers -1)))
  (pdf-view-mode-hook . pdf-tools-enable-minor-modes)
  :bind (:map pdf-view-mode-map
	      ("C-s" . isearch-forward-regexp)
	      ("j" . pdf-view-next-line-or-next-page)
	      ("k" . pdf-view-previous-line-or-previous-page))
)

(provide 'init-pdf)
