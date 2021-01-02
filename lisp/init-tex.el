(use-package auctex
  :defer t
  :config
  (load "preview-latex.el" nil t t)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  ;;  (setq-default TeX-engine 'xetex) ;;default engine
  ;;(setq-default TeX-PDF-mode t) ;;PDF output
  (setq-default TeX-master nil)
  (load "preview-latex.el" nil t t);;to preview latex
  (set-default 'preview-scale-function 3.0);;preview scale
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 3.0)) ;;preview in org-mode
  )

(add-hook 'LaTeX-mode-hook
          (lambda ()           
            (visual-line-mode 1)
	    (LaTeX-math-mode 1)
	    (display-line-numbers-mode -1)
	    (flyspell-mode 1)
	    (flycheck-mode -1)
	    ;;(variable-pitch-mode 1)
	    ))

(use-package magic-latex-buffer
  :hook
  (latex-mode . magic-latex-buffer)
  :config
  (setq magic-latex-enable-block-highlight nil
      magic-latex-enable-suscript        t
      magic-latex-enable-pretty-symbols  t
      magic-latex-enable-block-align     nil
      magic-latex-enable-inline-image    nil
      magic-latex-enable-minibuffer-echo nil))

;;tex preview
(use-package latex-preview-pane
  :defer t
  :config
  (setq doc-view-resolution 300) ;;make preview pdf clear
  (add-hook 'doc-view-mode-hook 'doc-view-fit-width-to-window)
  )

(setq TeX-view-program-list 
      '(("Sumatra PDF"
         ("\"c:/Program Files/SumatraPDF/SumatraPDF.exe\" -reuse-instance" 
          (mode-io-correlate " -forward-search \"%b\" %n ") " %o"))))

(setq TeX-view-program-selection
      '((output-dvi "DVI Viewer")
        (output-pdf "Sumatra PDF")))

(provide 'init-tex)
