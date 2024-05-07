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

(use-package svg-tag-mode
  :hook (org-mode . svg-tag-mode)
  :config
  (defface tab-bar-svg-active
    '((t (:foreground "#70797d")))
    "Tab bar face for selected tab.")

  (defface tab-bar-svg-inactive
    '((t (:foreground "#B0BEC5")))
    "Tab bar face for inactive tabs.")

  (defun my/tab-bar-auto-width (items)
    "Return tab-bar items with resized tab names."
    (unless tab-bar--auto-width-hash
      (define-hash-table-test 'tab-bar--auto-width-hash-test
                              #'equal-including-properties
                              #'sxhash-equal-including-properties)
      (setq tab-bar--auto-width-hash
            (make-hash-table :test 'tab-bar--auto-width-hash-test)))
    (let ((tabs nil)    ;; list of resizable tabs
          (non-tabs "") ;; concatenated names of non-resizable tabs
          (width 0))    ;; resize tab names to this width
      (dolist (item items)
	(when (and (eq (nth 1 item) 'menu-item) (stringp (nth 2 item)))
          (if (memq (get-text-property 0 'face (nth 2 item))
                    tab-bar-auto-width-faces)
              (push item tabs)
            (unless (eq (nth 0 item) 'align-right)
              (setq non-tabs (concat non-tabs (nth 2 item)))))))
      (when tabs
	(add-face-text-property 0 (length non-tabs) 'tab-bar t non-tabs)
	(setq width (/ (- (frame-inner-width)
                          (string-pixel-width non-tabs))
                       (length tabs)))
	(when tab-bar-auto-width-min
          (setq width (max width (if (window-system)
                                     (nth 0 tab-bar-auto-width-min)
                                   (nth 1 tab-bar-auto-width-min)))))
	(when tab-bar-auto-width-max
          (setq width (min width (if (window-system)
                                     (nth 0 tab-bar-auto-width-max)
                                   (nth 1 tab-bar-auto-width-max)))))
	(dolist (item tabs)
          (setf (nth 2 item)
		(with-memoization (gethash (list (selected-frame)
						 width (nth 2 item))
                                           tab-bar--auto-width-hash)
                  (let* ((name (nth 2 item))
			 (len (length name))
			 (close-p (get-text-property (1- len) 'close-tab name))
			 (continue t)
			 (prev-width (string-pixel-width name))
			 curr-width)
                    (cond
                     ((< prev-width width)
                      (let* ((space (apply 'propertize " "
                                           (text-properties-at 0 name)))
                             (ins-pos (- len (if close-p 1 0)))
                             (prev-name name))
			(while continue
                          (setf (substring name ins-pos ins-pos) space)
                          (setq curr-width (string-pixel-width name))
                          (if (and (< curr-width width)
                                   (> curr-width prev-width))
                              (setq prev-width curr-width
                                    prev-name name)
                            ;; Set back a shorter name
                            (setq name prev-name
                                  continue nil)))))
                     ((> prev-width width)
                      (let ((del-pos1 (if close-p -2 -1))
                            (del-pos2 (if close-p -1 nil)))
			(while continue
                          (setf (substring name del-pos1 del-pos2) "")
                          (setq curr-width (string-pixel-width name))
                          (if (and (> curr-width width)
                                   (< curr-width prev-width))
                              (setq prev-width curr-width)
                            (setq continue nil))))))
                    (propertize name 'display
				(svg-tag-make name (if (eq (car item) 'current-tab)
                                                       'tab-bar-svg-active
                                                     'tab-bar-svg-inactive))))))))
      items))

  (advice-add #'tab-bar-auto-width :override #'my/tab-bar-auto-width)

  (defun mk/svg-checkbox-empty ()
    (let* ((svg (svg-create 14 14)))
      (svg-rectangle svg 0 0 14 14 :fill 'white :rx 2 :stroke-width 2.5 :stroke-color 'black)
      (svg-image svg :ascent 'center)))

  (defun mk/svg-checkbox-filled ()
    (let* ((svg (svg-create 14 14)))
      (svg-rectangle svg 0 0 14 14 :fill "#FFFFFF" :rx 2)
      (svg-polygon svg '((5.5 . 11) (12 . 3.5) (11 . 2) (5.5 . 9) (1.5 . 5) (1 . 6.5))
		   :stroke-color 'black :stroke-width 1 :fill 'black)
      (svg-image svg :ascent 'center)))

  (defun mk/svg-checkbox-toggle ()
    (interactive)
    (save-excursion
      (let* ((start-pos (line-beginning-position))
	     (end-pos (line-end-position))
	     (text (buffer-substring-no-properties start-pos end-pos))
	     (case-fold-search t)  ; Let X and x be the same in search
	     )
	(beginning-of-line)
	(cond ((string-match-p "\\[X\\]" text)
	       (progn
		 (re-search-forward "\\[X\\]" end-pos)
		 (replace-match "[ ]")))
	      ((string-match-p "\\[ \\]" text)
	       (progn
		 (search-forward "[ ]" end-pos)
		 (replace-match "[X]")))))))

  (defun svg-progress-percent (value)
    (svg-image (svg-lib-concat
		(svg-lib-progress-bar (/ (string-to-number value) 100.0)
				      nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
		(svg-lib-tag (concat value "%")
			     nil :stroke 0 :margin 0)) :ascent 'center))

  (defun svg-progress-count (value)
    (let* ((seq (mapcar #'string-to-number (split-string value "/")))
	   (count (float (car seq)))
	   (total (float (cadr seq))))
      (svg-image (svg-lib-concat
		  (svg-lib-progress-bar (/ count total) nil
					:margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
		  (svg-lib-tag value nil
			       :stroke 0 :margin 0)) :ascent 'center)))

  (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
  (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
  (defconst day-re "[A-Za-z]\\{3\\}")
  (defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))

  (setq svg-tag-action-at-point 'edit)

  (setq svg-lib-icon-collections
	`(("bootstrap" .
	   "https://icons.getbootstrap.com/assets/icons/%s.svg")
	  ("simple" .
	   "https://raw.githubusercontent.com/simple-icons/simple-icons/develop/icons/%s.svg")
	  ("material" .
	   "https://raw.githubusercontent.com/Templarian/MaterialDesign/master/svg/%s.svg")
	  ("octicons" .
	   "https://raw.githubusercontent.com/primer/octicons/master/icons/%s-24.svg")
	  ("boxicons" .
	   "https://boxicons.com/static/img/svg/regular/bx-%s.svg")))

  (setq svg-tag-tags
	`(
	  ;; Task priority
	  ("\\[#[A-Z]\\]" . ((lambda (tag)
			       (svg-tag-make tag :face 'org-priority
					     :beg 2 :end -1 :margin 0))))

	  ;; Progress
	  ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
					      (svg-progress-percent (substring tag 1 -2)))))
	  ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
					    (svg-progress-count (substring tag 1 -1)))))

	  ;; Checkbox
	  ("\\[ \\]" . ((lambda (_tag) (mk/svg-checkbox-empty))
			(lambda () (interactive) (mk/svg-checkbox-toggle))
			"Click to toggle."))
	  ("\\(\\[[Xx]\\]\\)" . ((lambda (_tag) (mk/svg-checkbox-filled))
				 (lambda () (interactive) (mk/svg-checkbox-toggle))
				 "Click to toggle."))

	  ;; Active date (with or without day name, with or without time)
	  (,(format "\\(<%s>\\)" date-re) .
	   ((lambda (tag)
	      (svg-tag-make tag :beg 1 :end -1 :margin 0))))
	  (,(format "\\(<%s \\)%s>" date-re day-time-re) .
	   ((lambda (tag)
	      (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
	  (,(format "<%s \\(%s>\\)" date-re day-time-re) .
	   ((lambda (tag)
	      (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))

	  ;; Inactive date  (with or without day name, with or without time)
	  (,(format "\\(\\[%s\\]\\)" date-re) .
	   ((lambda (tag)
	      (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
	  (,(format "\\(\\[%s \\)%s\\]" date-re day-time-re) .
	   ((lambda (tag)
	      (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))
	  (,(format "\\[%s \\(%s\\]\\)" date-re day-time-re) .
	   ((lambda (tag)
	      (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date))))

	  ;; Keywords
	  ("TODO" . ((lambda (tag) (svg-tag-make tag :height 0.8 :inverse t
						 :face 'org-todo :margin 0 :radius 5))))
	  ("WORK" . ((lambda (tag) (svg-tag-make tag :height 0.8
						 :face 'org-todo :margin 0 :radius 5))))
	  ("DONE" . ((lambda (tag) (svg-tag-make tag :height 0.8 :inverse t
						 :face 'org-done :margin 0 :radius 5))))

	  ("FIXME\\b" . ((lambda (tag) (svg-tag-make "FIXME" :face 'org-todo :inverse t :margin 0 :crop-right t))))

	  ;; beautify pagebreak in orgmode
	  ("\\\\pagebreak" . ((lambda (tag) (svg-lib-icon "file-break" nil :collection "bootstrap"
							  :stroke 0 :scale 1 :padding 0)))))))


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

(use-package org-modern
  :custom
  (org-modern-hide-stars nil)		; adds extra indentation
  (org-modern-table nil)
  (org-modern-list 
   '(;; (?- . "-")
     (?* . "•")
     (?+ . "‣")))
  (org-modern-block-name '("" . "")) ; or other chars; so top bracket is drawn promptly
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda))

(use-package org-modern-indent
  :straight (org-modern-indent :type git :host github :repo "jdtsmith/org-modern-indent")
  :hook
  (org-mode . org-modern-indent-mode))

;; AUCTeX
(use-package tex
  :straight (:type built-in)
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
	TeX-view-program-selection '((output-pdf "Skim"))
	)
  )

;; CDLaTeX
;; https://karthinks.com/software/latex-input-for-impatient-scholars/#step-2-cdlatex
(use-package cdlatex
  :hook (LaTeX-mode . cdlatex-mode))

(use-package citar
  :custom
  (citar-bibliography '("/Users/yingrui/Library/ZoteroLibrary/library.bib"))
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup))

(use-package auctex-latexmk
  :after tex
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
