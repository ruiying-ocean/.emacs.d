(use-package tex ;;not auctex instead!
  :ensure auctex
  :defer t
  :mode ("\\.tex\\'" . latex-mode)
  :config
  (setq TeX-auto-save t
        TeX-parse-self t)
  (setq-default TeX-engine 'xetex) ;;default engine
  (setq-default TeX-PDF-mode t) ;;PDF output
  (setq-default TeX-master nil)

  ;;Preview latex C-c C-p C-p
  (setq preview-pdf-color-adjust-method t)
  (set-default 'preview-scale-function 1.0);;preview scale
  ;;  (setq org-format-latex-options (plist-put org-format-latex-options :scale 3.0)) ;;preview in org-mode
  ;; (custom-set-faces 
  ;;  '(preview-reference-face ((t (:background "gray" :foreground "black")))))


  ;;sync latex <-> pdf
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer);;auto revert PDF buffer

  ;;C-c C-v to sync forward, double click to sync backward
  (setq TeX-view-program-selection '((output-pdf "PDF Tools")
				     (output-dvi "DVI Viewer"))
	TeX-source-correlate-start-server t
	TeX-source-correlate-method 'auto) ;;Method to use for enabling forward and inverse search

  (add-hook 'LaTeX-mode-hook
            (lambda ()           
              (visual-line-mode -1)
	      (visual-fill-column-mode -1)
	      (LaTeX-math-mode 1)
	      (display-line-numbers-mode 1)
	      (flyspell-mode 1)
	      (flycheck-mode -1)
	      ;;(variable-pitch-mode 1)
	      (TeX-source-correlate-mode 1) ;;Needed to sync TeX and PDF
	      ))
  )

(use-package magic-latex-buffer
  :defer t
  :hook
  (LaTeX-mode . magic-latex-buffer)
  :config
  (setq magic-latex-enable-block-highlight nil
      magic-latex-enable-suscript        t
      magic-latex-enable-pretty-symbols  t
      magic-latex-enable-block-align     nil
      magic-latex-enable-inline-image    nil
      magic-latex-enable-minibuffer-echo nil))

;; Retrieve BibTeX entries
;; Call 'gscholar-bibtex' to retrieve BibTeX entries from Google
;; Scholar, ACM Digital Library, IEEE Xplore and DBLP.
(use-package gscholar-bibtex
  :defer t)

;; Reformat BibTeX using bibclean
(use-package bibclean-format
  :defer t
  :bind (:map bibtex-mode-map
         ("C-c f" . bibclean-format)))

(provide 'init-tex)
