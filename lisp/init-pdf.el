;;allow you to view pdf continuously
(use-package pdf-continuous-scroll-mode
  :defer t
  :load-path "extra/"
  :hook
  (pdf-view-mode-hook . pdf-continuous-scroll-mode))

;; one way to download using qulepa, but too slow and annoying for me
;; (use-package pdf-continuous-scroll-mode
;;   :defer t
;;   :quelpa
;;   (pdf-continuous-scroll-mode :fetcher github
;; 			      :repo "dalanicolai/pdf-continuous-scroll-mode.el"))

;;read the documentation to find how to compile and pdf-tools first
(use-package pdf-tools
  :defer t
  :commands (pdf-view-mode pdf-loader-install)
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query)
  (define-pdf-cache-function pagelables)
  ;;In case of high-resolution screen like Mac
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil)
  :hook
  (pdf-view-mode-hook . (lambda () (display-line-numbers -1)))
  (pdf-view-mode-hook . pdf-tools-enable-minor-modes)
  :bind (:map pdf-view-mode-map
	      ("C-s" . isearch-forward-regexp)
	      ("j" . pdf-view-next-line-or-next-page)
	      ("k" . pdf-view-previous-line-or-previous-page))
)

(provide 'init-pdf)
