;;; This file is setting of all packages in use-package framework
(setq use-package-always-ensure t)

(use-package benchmark-init
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package company
  :config
  (setq company-idle-delay 0.5)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode 1))

(use-package company-tabnine
  :ensure t 
  :after 'company-mode 
  'company-tabnine-mode 
  :config (add-to-list 'company-backends #'company-tabnine)
  (setq company-idle-delay 0)
  (setq company-show-numbers t)
  )

(use-package yasnippet
  :ensure yasnippet-snippets
  :hook (after-init . yas-global-mode)
  )

(use-package transient
  :ensure t)

(use-package magit
  :bind (("C-x g" . magit-status)))

(use-package exec-path-from-shell
   :if (memq window-system '(mac ns))
   :ensure t
   :config
   (exec-path-from-shell-initialize))

(use-package popwin
  :init (require 'popwin)
  :config
  (popwin-mode t))

(use-package nyan-mode
  :config
  (nyan-mode t))

(use-package which-key
  :config
  (which-key-mode))

(use-package projectile
  :defer 3
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (projectile-mode +1)
  :bind
  (:map projectile-mode-map
	("C-c p" . projectile-command-map)))

(use-package flycheck
  :init (global-flycheck-mode))

(use-package rainbow-delimiters
  :config
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))

(use-package all-the-icons-dired
  ;need to run all-the-icons-install-fonts first to avoid grabled icon
  :requires all-the-icons  
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package neotree
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (global-set-key [f8] 'neotree-toggle)
  )

(use-package highlight-indent-guides
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

;(use-package spaceline
;  :init
;  (require 'spaceline-config)
;  :config
;  (spaceline-emacs-theme))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(use-package awesome-tab
  ;;not currently available on melpa
  :load-path "~/.emacs.d/config/awesome-tab.el"
  :config
  (awesome-tab-mode t)
  (setq awesome-tab-show-tab-index t)
  (setq awesome-tab-height 100)
  )

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package smex
  :init (smex-initialize)
  :bind (("M-x" . smex)
	 ("M-x" . smex-major-mode-commands)))

; (use-package auctex
;   :defer t
;   :config
;   (setq TeX-auto-save t)
;   (setq TeX-parse-self t)
;   (setq-default TeX-master nil)
;   (setq TeX-PDF-mode t)
;   (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
;   )

; (use-package latex-preview-pane
;   :defer t
;   :config
;   (setq doc-view-resolution 300) ;;make preview pdf clear
;   (add-hook 'doc-view-mode-hook 'doc-view-fit-width-to-window)
;   )

(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (global-set-key "\C-s" 'swiper)
)

(use-package counsel
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-h f") 'counsel-describe-function)
  (global-set-key (kbd "C-h v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(use-package avy
  ;;快速跳转字符或行
  :defer 2
  :config
  (global-set-key (kbd "C-:") 'avy-goto-char)
  (global-set-key (kbd "C-'") 'avy-goto-char-2)
  (global-set-key (kbd "M-g w") 'avy-goto-word-1)
  (global-set-key (kbd "M-g l") 'avy-goto-line))

(use-package elpy
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  (add-hook 'python-mode-hook 'eldoc-mode)
  (setq elpy-rpc-python-command "python3")
  (setq elpy-shell-echo-output nil) ;; to solve the ^G^G^G bug
  (setq python-shell-completion-native-enable nil)
  )

(use-package pydoc
  :bind ("C-c C-d" . pydoc)
  )

 (use-package company-jedi
   :config
   (defun my/python-mode-hook ()
   (add-to-list 'company-backends 'company-jedi))
   (add-hook 'python-mode-hook 'my/python-mode-hook)
   )

;;grip mode need to run pip install grip first
(use-package grip-mode
  :bind (:map markdown-mode-command-map
         ("g" . grip-mode)))

; (use-package ess-r-mode
;   :ensure ess
;   :config
;   (defun then_R_operator ()
;   "R - %>% operator or 'then' pipe operator"
;   (interactive)
;   (just-one-space 1)
;   (insert "%>%")
;   (reindent-then-newline-and-indent))
;   :bind
;   (:map ess-mode-map
;         ("M-=" . ess-cycle-assign)
;         ("M-p" . then_R_operator))
;   (:map inferior-ess-mode-map
;         ("M-=" . ess-cycle-assign)
; 	("M-p" . then_R_operator))
;   )

; (use-package calfw
;   :ensure calfw-org
;   :init
;   (require 'calfw-org)
;   ;:config
;   ;(setq cfw:org-overwrite-default-keybinding t)
;   :bind
;   (:map org-mode-map
; 	("C-c v" . cfw:open-org-calendar)))

; (use-package org-bullets
;   :config
;   (add-hook 'org-mode-hook 'org-bullets-mode))

; (use-package org
;   :defer t
;   :config
;   ;; 默认开启标题缩进
;   (setq org-startup-indented t)
;   (add-to-list 'file-coding-system-alist
; 	       '("\\.org" . utf-8))
;   ;; todo keywords 背景色
;   (setf org-todo-keyword-faces '(("TODO" . (:foreground "white" :background "red"   :weight bold))
;                                 ("HAND" . (:foreground "white" :background "#2E8B57"  :weight bold))
;                                 ("DONE" . (:foreground "white" :background "#3498DB" :weight bold))))
;   (org-agenda-to-appt t);;事件提醒
;   (setq org-src-fontify-natively t) ;;高亮org代码块
;   ;;设置关键词
;   (setq org-todo-keywords
; 	'((sequence "TODO" "DOING"  "|" "DONE" "CANCELED")))
;   (add-hook 'org-mode-hook 'org-toggle-pretty-entities)  ;; 默认开启自动转换特殊字符及数学公式模式
; ;;agenda色块函数
;   (defun my-org-agenda-time-grid-spacing ()
;   "Set different line spacing w.r.t. time duration."
;   (save-excursion
;     (let* ((background (alist-get 'background-mode (frame-parameters)))
;            (background-dark-p (string= background "dark"))
;            (colors (list "#1ABC9C" "#2ECC71" "#3498DB" "#9966ff"))
;            pos
;            duration)
;       (nconc colors colors)
;       (goto-char (point-min))
;       (while (setq pos (next-single-property-change (point) 'duration))
;         (goto-char pos)
;         (when (and (not (equal pos (point-at-eol)))
;                    (setq duration (org-get-at-bol 'duration)))
;           (let ((line-height (if (< duration 30) 1.0 (+ 0.5 (/ duration 60))))
;                 (ov (make-overlay (point-at-bol) (1+ (point-at-eol)))))
;             (overlay-put ov 'face `(:background ,(car colors)
;                                                 :foreground
;                                                 ,(if background-dark-p "black" "white")))
;             (setq colors (cdr colors))
;             (overlay-put ov 'line-height line-height)
;             (overlay-put ov 'line-spacing (1- line-height))))))))
;   (add-hook 'org-agenda-finalize-hook #'my-org-agenda-time-grid-spacing)
;   (setq org-capture-templates
; 	'(("t" "Todo" entry (file+headline "~/.emacs.d/org/inbox.org" "Tasks")
; 	   "* TODO %?\n  %i\n  %a")
; 	  ("j" "Journal" entry (file+datetree "~/.emacs.d/org/journal.org")
;          "* %?\nEntered on %U\n  %i\n  %a")))
;   (setq org-default-notes-file "~/.emacs.d/org/inbox.org")
;   (setq org-archive-location "~/.emacs.d/org/achives.org::* From %s")
;   (setq org-agenda-files (list  "~/.emacs.d/org/agenda.org"))
; ;;(setq org-refile-targets '(("~/.emacs.d/org/agenda.org" :level . 1)))
;   ;;You don't need refile 'cause you have only one org-agenda-file
;   (org-babel-do-load-languages 'org-babel-load-languages
; 			       '((emacs-lisp . t)
; 				 (python . t)
; 				 (R . t)
; 				 ));;then C-c C-c can run this code block
;   :bind
;   (:map org-mode-map
; 	("C-c C-e" . org-edit-src-code)
; 	("C-c a" . org-agenda)
; 	("C-c c" . org-capture)
; 	("C-c C-r" . org-archive-subtree))
;   )

; (use-package org-pomodoro
;   :bind	("C-x p" . org-pomodoro)
;   :config
;   (setq org-agenda-clockreport-parameter-plist '(:fileskip0 t :link t :maxlevel 2 :formula "$5=($3+$4)*(60/25);t"))
;   (setq org-clock-sound t)
;   )

(provide 'package-configs.el)
;;; package-configs.el ends here
