;; (use-package org-bullets
;;   :config
;;   (add-hook 'org-mode-hook 'org-bullets-mode))

;;use org-superstar-mode to replace org-bullets
(use-package org-superstar
  :defer t
  :config
  (setq org-superstar-special-todo-items t)
  :hook
  (org-mode . org-superstar-mode)
  :custom
  (org-ellipsis "⤵"))

(use-package org-fancy-priorities
  :defer t
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕"))
  (unless (char-displayable-p ?❗)
    (setq org-fancy-priorities-list '("HIGH" "MID" "LOW" "OPTIONAL"))))

(setq-default ispell-program-name "~/Hunspell/bin/hunspell.exe") ;;need to donwload hunspell and add to path
(setq ispell-local-dictionary "en_GB")

(add-hook 'org-mode-hook
          (lambda ()
            (variable-pitch-mode 1)
            (visual-line-mode 1)
	    (display-line-numbers-mode -1)
	    (flyspell-mode 1)
	    ))

;;todo keywords 背景色
;; (setf org-todo-keyword-faces '(("TODO" . (:foreground "white" :background "red"   :weight bold))
;;                               ("HAND" . (:foreground "white" :background "#2E8B57"  :weight bold))
;;                               ("DONE" . (:foreground "white" :background "#3498DB" :weight bold))))

;;层级项目
(setq org-list-demote-modify-bullet
      (quote (("+" . "-")
              ("-" . "+")
              ("*" . "-")
              ("1." . "-")
              ("1)" . "-")
              ("A)" . "-")
              ("B)" . "-")
              ("a)" . "-")
              ("b)" . "-")
              ("A." . "-")
              ("B." . "-")
              ("a." . "-")
              ("b." . "-"))))

(use-package org
  :defer t
  :config
  ;; 默认开启标题缩进
  (setq org-startup-indented t)
;;  (org-agenda-to-appt t);;事件提醒
  (setq org-src-fontify-natively t) ;;高亮org代码块
  ;;设置关键词
  (setq org-todo-keywords
	'((sequence "TODO" "DOING"  "|" "DONE" "CANCELED")))
  (add-hook 'org-mode-hook 'org-toggle-pretty-entities)  ;; 默认开启自动转换特殊字符及数学公式模式
;;agenda色块函数
  (defun my-org-agenda-time-grid-spacing ()
  "Set different line spacing w.r.t. time duration."
  (save-excursion
    (let* ((background (alist-get 'background-mode (frame-parameters)))
           (background-dark-p (string= background "dark"))
           (colors (list "#1ABC9C" "#2ECC71" "#3498DB" "#9966ff"))
           pos
           duration)
      (nconc colors colors)
      (goto-char (point-min))
      (while (setq pos (next-single-property-change (point) 'duration))
        (goto-char pos)
        (when (and (not (equal pos (point-at-eol)))
                   (setq duration (org-get-at-bol 'duration)))
          (let ((line-height (if (< duration 30) 1.0 (+ 0.5 (/ duration 60))))
                (ov (make-overlay (point-at-bol) (1+ (point-at-eol)))))
            (overlay-put ov 'face `(:background ,(car colors)
                                                :foreground
                                                ,(if background-dark-p "black" "white")))
            (setq colors (cdr colors))
            (overlay-put ov 'line-height line-height)
            (overlay-put ov 'line-spacing (1- line-height))))))))
  (add-hook 'org-agenda-finalize-hook #'my-org-agenda-time-grid-spacing)
  (setq org-capture-templates
	'(("t" "Todo" entry (file+headline "~/.emacs.d/org/inbox.org" "Tasks")
	   "* TODO %?\n  %i\n  %a")
	  ("j" "Journal" entry (file+datetree "~/.emacs.d/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")))
  (setq org-default-notes-file "~/.emacs.d/org/inbox.org")
  (setq org-archive-location "~/.emacs.d/org/achives.org::* From %s")
  (setq org-agenda-files (list  "~/.emacs.d/org/agenda.org"))
;;(setq org-refile-targets '(("~/.emacs.d/org/agenda.org" :level . 1)))
  ;;You don't need refile 'cause you have only one org-agenda-file
  (org-babel-do-load-languages 'org-babel-load-languages
			       '((emacs-lisp . t)
				 (python . t)
				 (R . t)
				 ));;then C-c C-c can run this code block
  :bind
  (:map org-mode-map
	("C-c a" . org-agenda)
	("C-c c" . org-capture)
	("C-c C-r" . org-archive-subtree))
  )

(use-package org-num
  :load-path "lisp/"
  :after org
  :hook (org-mode . org-num-mode))

(setq org-hide-emphasis-markers t)
(setq inhibit-compacting-font-caches t)

;; (let* ((variable-tuple (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
;;                              ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
;;                              ((x-list-fonts "Verdana")         '(:font "Verdana"))
;;                              ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
;;                              (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
;;        (base-font-color     (face-foreground 'default nil 'default))
;;        (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

;;         (custom-theme-set-faces 'user
;;                           `(org-level-8 ((t (,@headline ,@variable-tuple))))
;;                           `(org-level-7 ((t (,@headline ,@variable-tuple))))
;;                           `(org-level-6 ((t (,@headline ,@variable-tuple))))
;;                           `(org-level-5 ((t (,@headline ,@variable-tuple))))
;;                           `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
;;                           `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
;;                           `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
;;                           `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
;;                           `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil))))))
                          
(custom-theme-set-faces
 'user
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(variable-pitch ((t (:family "ETBembo" :height 140 :weight thin))))
 '(fixed-pitch ((t ( :family "Fira Code" :height 120)))) 
)


; (use-package calfw
;   :ensure calfw-org
;   :init
;   (require 'calfw-org)
;   ;:config
;   ;(setq cfw:org-overwrite-default-keybinding t)
;   :bind
;   (:map org-mode-map
; 	("C-c v" . cfw:open-org-calendar)))

; (use-package org-pomodoro
;   :bind	("C-x p" . org-pomodoro)
;   :config
;   (setq org-agenda-clockreport-parameter-plist '(:fileskip0 t :link t :maxlevel 2 :formula "$5=($3+$4)*(60/25);t"))
;   (setq org-clock-sound t)
;;   )

(provide 'init-org)
