;;AUCTeX is the main package
;;LaTeX preview pane package to preview
;;pdftools as pdf viewer, but require autoconf,automake,C/Cpp compiler. But I don't want so many stuffs on stupid windows OS.
;;more info see in https://github.com/politza/pdf-tools
;;the default pdf viewer is doc-view mode,but it require ghostscript something, it convert pdf to png file and show it, I don't like it
;;yasnippets as TeX template
(load "auctex.el" nil t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(add-hook 'latex-mode-hook #'rainbow-delimiters-mode)

(provide 'tex-config)
