(use-package auctex
  :defer t
  :config
  (load "preview-latex.el" nil t t)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)  
  (load "preview-latex.el" nil t t);;to preview latex
)

(add-hook 'LaTeX-mode-hook
          (lambda ()           
            (visual-line-mode 1)
	    (LaTeX-math-mode 1)
	    (display-line-numbers-mode -1)
	    (flyspell-mode 1)
	    ))

(setq TeX-view-program-list 
      '(("Sumatra PDF"
         ("\"c:/Program Files/SumatraPDF/SumatraPDF.exe\" -reuse-instance" 
          (mode-io-correlate " -forward-search \"%b\" %n ") " %o"))))

(setq TeX-view-program-selection
      '((output-dvi "DVI Viewer")
        (output-pdf "Sumatra PDF")))

; (use-package latex-preview-pane
;   :defer t
;   :config
;   (setq doc-view-resolution 300) ;;make preview pdf clear
;   (add-hook 'doc-view-mode-hook 'doc-view-fit-width-to-window)
;   )

(provide 'init-tex)
