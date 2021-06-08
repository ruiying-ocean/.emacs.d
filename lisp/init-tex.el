(use-package auctex
  :defer t
  :hook (LaTeX-mode)
  :config
  (load "preview-latex.el" nil t t)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-engine 'xetex) ;;default engine
  (setq-default TeX-PDF-mode t) ;;PDF output
  (setq-default TeX-master nil)
  (load "preview-latex.el" nil t t);;to preview latex
  (set-default 'preview-scale-function 3.0);;preview scale
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 3.0)) ;;preview in org-mode
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer);;auto revert PDF buffer
  )

(add-hook 'LaTeX-mode-hook
          (lambda ()           
            (visual-line-mode -1)
	    (visual-fill-column-mode -1)
	    (LaTeX-math-mode 1)
	    (display-line-numbers-mode 1)
	    (flyspell-mode 1)
	    (flycheck-mode -1)
	    ;;(variable-pitch-mode 1)
	    ))

(use-package magic-latex-buffer
  :defer t
  :hook
  (latex-mode . magic-latex-buffer)
  :config
  (setq magic-latex-enable-block-highlight nil
      magic-latex-enable-suscript        t
      magic-latex-enable-pretty-symbols  t
      magic-latex-enable-block-align     nil
      magic-latex-enable-inline-image    nil
      magic-latex-enable-minibuffer-echo nil))


;;Add some external PDF programs
(setq TeX-view-program-list '(("Evince" "evince --page-index=%(outpage) %o")))

;;Recommend using PDF tools to view tex output in Emacs!
;;C-c C-v to sync forward, double click to sync backward
(setq TeX-view-program-selection
      '((output-dvi "DVI Viewer")
        (output-pdf "PDF Tools")))

(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
(setq TeX-source-correlate-start-server t)

(provide 'init-tex)
