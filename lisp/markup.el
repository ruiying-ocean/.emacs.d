;;; Text-mode: Markdown/org-mode/TeX

;;;;;;;;;;;;;;
;; Markdown ;;
;;;;;;;;;;;;;;

;; Major mode for markdown
;; preview included but reply on multimarkdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode)
	 ("\\.Rmarkdown\\'" . markdown-mode)
	 )
  :custom
  (markdown-enable-math t)
  :init
  (setq markdown-command "multimarkdown")
  :hook
  (markdown-mode . (lambda ()
		     (display-line-numbers-mode -1)
		     (visual-line-mode 1))))

;; Advanced preview
(use-package emacs-livedown
  :ensure-system-package (livedown . "npm install -g livedown")
  :after markdown-mode
  :straight (:type git
		   :host github
		   :repo "shime/emacs-livedown")
  :custom
  (livedown-autostart t) ; automatically open preview when opening markdown files
  (livedown-open t)	 ; automatically open the browser window
  (livedown-port 1337)	 ; port for livedown server
  (livedown-browser nil) ; browser to use
  :bind
  (:map markdown-mode-map
	("C-c c p" . livedown-preview)
	("C-c c k" . livedown-kill)))

;;add table of content for md/org
;;Add :TOC: tag for org (C-c C-c) and <-- :TOC: --> for md
;;then toc-org-insert-toc
(use-package toc-org
  :hook
  (markdown-mode . toc-org-mode)
  (org-mode . toc-org-mode)
  :config
  (global-set-key (kbd "\C-c\C-o") 'toc-org-markdown-follow-thing-at-point)
  (add-to-list 'org-tag-alist '("TOC" . ?T)))

;; quarto support
;; Or, with use-package:
(use-package quarto-mode
  :mode ("\\.qmd\\'" . quarto-mode))



;;===========
;; Org-mode
;;===========

(use-package org
  :straight (:type built-in)
  :custom
  ;; set default note file
  (org-directory "~/Documents")
  (org-default-notes-file (concat org-directory "/TODO.org"))

  :custom-face
  ;; source code block line
  (org-block-begin-line ((t (:underline t :background unspecified))))
  (org-block-end-line ((t (:overline t :underline nil :background unspecified))))

  :config
  (setq org-hide-block-startup t)
  ;; learn from: https://github.com/Elilif/.elemacs
  (defun eli-hide-org-block-begin-line (orig from to flag spec)
    (if (eq spec 'org-hide-block)
	(let* ((beg-of-line (save-excursion
			      (beginning-of-line)
			      (point)))
	       (lang (car (org-babel-get-src-block-info)))
	       (beg (+ beg-of-line 12 (length lang))))
	  (funcall orig beg to flag spec))
      (funcall orig from to flag spec)))

  (advice-add 'org-flag-region :around #'eli-hide-org-block-begin-line)

  (setq org-todo-keywords
	'((sequence "TODO(t)" "DOING(s)" "|" "DONE(d!/!)")))

  ;; capture TODOs: %U -> dat; %i -> current selection;
  (setq org-capture-templates
	'(("t" "Todo" entry (file+headline "~/Documents/TODO.org" "Tasks")
	   "* TODO [#A] %? %i %U"
	   :empty-lines 1)))

  (setq org-fontify-quote-and-verse-blocks t)


  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (shell . t)))


  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :weight bold))))
   '(org-level-2 ((t (:inherit outline-2 :weight normal))))
   '(org-level-3 ((t (:inherit outline-3 :weight normal))))
   '(org-level-4 ((t (:inherit outline-4 :weight normal))))
   '(org-level-5 ((t (:inherit outline-5 :weight normal))))
   '(org-level-6 ((t (:inherit outline-6 :weight normal))))
   '(org-level-7 ((t (:inherit outline-7 :weight normal))))
   '(org-level-8 ((t (:inherit outline-8 :weight normal)))))

  ;; emphasize
  (defface org-bold
    '((t :foreground "#d2268b"
	 :background "#fefefe"
	 :weight bold
	 :underline t
	 :overline t))
    "Face for org-mode bold."
    :group 'org-faces)

  (setq org-emphasis-alist
	'(("*" org-bold)
          ("/" italic)
          ("_" underline)
          ("=" (:background "maroon" :foreground "white") org-verbatim)
          ("~" (:background "deep sky blue" :foreground "MidnightBlue") org-code)
          ("+" (:strike-through t) org-code)))

  (set-face-background 'org-bold "#fefefe")
  (set-face-background 'org-verbatim "#fefefe")

  :bind
  (:map org-mode-map
	("C-c s" . org-insert-structure-template)
	;; Create the new heading
	("C-c C-w" . org-refile))

  :hook
  ;; pretty symbol for org-mode
  (org-mode . (lambda ()
		;; tickboxes
		(push '("[ ]" . "🞎") prettify-symbols-alist)
		(push '("[X]" . "☑") prettify-symbols-alist)
		(push '("[-]" . "◫") prettify-symbols-alist)
		(push '("!=" . "≠") prettify-symbols-alist)
		;; arrows
		(push '("->" . "→") prettify-symbols-alist)
		(push '("<-" . "←") prettify-symbols-alist)
		(push '("=>" . "⇒") prettify-symbols-alist)
		(push '("<=" . "⇐") prettify-symbols-alist)
		(push '("\\->" . "↳") prettify-symbols-alist)
		(push '("<-/" . "↵") prettify-symbols-alist)
		(prettify-symbols-mode))))


;; paste from clipboard
;; require: brew install pngpaste poppler
(use-package org-mac-image-paste
  :straight (:host github :repo "jdtsmith/org-mac-image-paste")
  :config (org-mac-image-paste-mode 1)
  :hook
  (org-mode . org-mac-image-paste-mode))

(use-package org-download)

;; Beautify org-mode
(use-package org-superstar
  :hook
  (org-mode . org-superstar-mode))

;; Perfectly alian English/CJK fonts in the same table
(use-package valign
  :hook (org-mode . valign-mode)
  :config
  (setq valign-fancy-bar t))

;; A dictionary inside Emacs, by abo-abo!
(use-package define-word
  :bind
  ("C-c d" . define-word-at-point)
  :config
  (setq define-word-default-service 'webster))

(use-package org-modern-indent
  :straight (org-modern-indent :type git :host github :repo "jdtsmith/org-modern-indent")
  :hook
  (org-mode . org-modern-indent-mode))

;; AUCTeX

;; AUCTEX
;; all in one: C-c C-c
;; (use-package auctex
;;   :ensure t
;;   :no-require t
;;   :config
;;   ;; get support for many of the LaTeX packages in your documents
;;   (setq TeX-auto-save t)
;;   (setq TeX-parse-self t)

;;   ;; pdf viewer: Skim
;;   (setq TeX-view-program-list
;; 	'(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))

;;   (setq TeX-view-program-selection '((output-pdf "Skim")))
;;   ;; Forward search happens automatically upon calling the viewer,
;;   (setq TeX-source-correlate-method 'synctex)
;;   :hook
;;   ;; SyncTeX setup
;;   (LaTeX-mode . TeX-source-correlate-mode)
;;   :bind
;;   (:map LaTeX-mode-map
;; 	("C-c C-a" . TeX-command-run-all)
;; 	("M-<mouse-1>" . TeX-view)))

(use-package tex
  :straight nil 
  ;; :load-path ("~/.emacs.d/lisp/auctex")
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-engine 'xetex) ;; Use xetex by default.
  (setq TeX-save-query nil) ;; Save without asking.
  (setq TeX-master nil) ;; Query for master file.

  (setq TeX-source-correlate-method 'synctex
	TeX-view-program-list
	'(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -g -b %n %o %b"))
	TeX-view-program-selection '((output-pdf "Skim")))

  ;; enable outline, C-c @ to toggle outline
  (add-hook 'LaTeX-mode-hook #'outline-minor-mode)  
  )


(add-hook 'latex-mode-hook
          (lambda ()
            (setq outline-regexp "\\\\[a-zA-Z]+")
            (setq outline-level 'my-outline-level)))

(defun my-outline-level ()
  (interactive)
  (let ((str (match-string 0)))
    (cond
     ((equal str "\\chapter") 1)
     ((equal str "\\section") 2)
     ((equal str "\\subsection") 3)
     ((equal str "\\subsubsection") 4)
     ((equal str "\\paragraph") 5)
     ((equal str "\\subparagraph") 6)
     (t 7))))

;; change the outline faces
(use-package outline-minor-faces
  :after outline
  :config (add-hook 'outline-minor-mode-hook
                    #'outline-minor-faces-mode))


;; one key to compile and view
;; C-c C-a => latex/compile-commands-until-done
(use-package latex-extra
  :hook
  (LaTeX-mode . latex-extra-mode)
  :config
  ;; unset C-c C-f in latex-extra
  (define-key latex-extra-mode-map (kbd "C-c C-f") nil))

;; CDLaTeX
;; https://karthinks.com/software/latex-input-for-impatient-scholars/#step-2-cdlatex
(use-package cdlatex
  :hook (LaTeX-mode . cdlatex-mode))

(use-package auctex-latexmk
  ;; :after tex
  :custom
  (auctex-latexmk-inherit-TeX-PDF-mode t)
  :config
  (auctex-latexmk-setup))

;; support for quarto
(use-package quarto-mode
  :mode (("\\.qmd" . poly-quarto-mode))
  ;;  :hook (visual-line-mode . poly-quarto-mode)
  )

(provide 'markup)
