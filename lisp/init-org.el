;; Beautify org-mode
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
  (setq org-fancy-priorities-list '((?A . "❗")
                                  (?B . "⬆")
                                  (?C . "⬇")
                                  (?D . "☕")
                                  (?1 . "⚡")
                                  (?2 . "⮬")
                                  (?3 . "⮮")
                                  (?4 . "☕")
                                  (?I . "Important")))
  )

(use-package org-num
  :load-path "lisp/"
  :after org
  :hook (org-mode . org-num-mode))

(setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "λ")
                                       ("#+END_SRC" . "λ")
                                       ("#+begin_src" . "λ")
                                       ("#+end_src" . "λ")
				       ("[ ]" .  "☐")
				       ("[X]" . "☑" )
				       ("[-]" . "❍" )
				       ))
(setq prettify-symbols-unprettify-at-point 'right-edge)
(add-hook 'org-mode-hook 'prettify-symbols-mode)

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
              ("b)" . "-"))))

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

(setq org-hide-emphasis-markers t)
(setq inhibit-compacting-font-caches t)
(defun modify-org-done-face()
  (setq org-fontify-done-headline t)
  (set-face-attribute 'org-done nil :strike-through t)
  (set-face-attribute 'org-headline-done nil
                      :strike-through t
                      :foreground "#363636"))
(eval-after-load "org"
  (add-hook 'org-add-hook 'modify-org-done-face))

;;org font-face setting, if not work well, disable variable-pitch-mode
(custom-theme-set-faces
 'user
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
 '(org-link ((t (:foreground "steel blue" :underline t))))
)

;; use flyspell in org-mode
(setq-default ispell-program-name "~/Hunspell/bin/hunspell.exe") ;;require donwloading hunspell and add to path
(setq ispell-local-dictionary "en_GB")

(add-hook 'org-mode-hook
          (lambda ()
;;	    (variable-pitch-mode 1)	;;need to manually turn on now
            (visual-line-mode 1)
	    (display-line-numbers-mode -1)
	    (flyspell-mode 1)
	    ))

(use-package org-super-agenda
  :defer t
  :config
  (let ((org-super-agenda-groups
       '(;; Each group has an implicit boolean OR operator between its selectors.
         (:name "Today"  ; Optionally specify section name
                :time-grid t  ; Items that appear on the time grid
                :todo "TODAY")  ; Items that have this TODO keyword
         (:name "Important"
                ;; Single arguments given alone
                :tag "bills"
                :priority "A")
         ;; Set order of multiple groups at once
         (:order-multi (2 (:name "Shopping in town"
                                 ;; Boolean AND group matches items that match all subgroups
                                 :and (:tag "shopping" :tag "@town"))
                          (:name "Food-related"
                                 ;; Multiple args given in list with implicit OR
                                 :tag ("food" "dinner"))
                          (:name "Personal"
                                 :habit t
                                 :tag "personal")
                          (:name "Space-related (non-moon-or-planet-related)"
                                 ;; Regexps match case-insensitively on the entire entry
                                 :and (:regexp ("space" "NASA")
                                               ;; Boolean NOT also has implicit OR between selectors
                                               :not (:regexp "moon" :tag "planet")))))
         ;; Groups supply their own section names when none are given
         (:todo "WAITING" :order 8)  ; Set order of this section
         (:todo ("SOMEDAY" "TO-READ" "CHECK" "TO-WATCH" "WATCHING")
                ;; Show this group at the end of the agenda (since it has the
                ;; highest number). If you specified this group last, items
                ;; with these todo keywords that e.g. have priority A would be
                ;; displayed in that group instead, because items are grouped
                ;; out in the order the groups are listed.
                :order 9)
         (:priority<= "B"
                      ;; Show this section after "Today" and "Important", because
                      ;; their order is unspecified, defaulting to 0. Sections
                      ;; are displayed lowest-number-first.
                      :order 1)
         ;; After the last group, the agenda will display items that didn't
         ;; match any of these groups, with the default order position of 99
         )))
  (org-agenda nil "a"))
  )

(use-package org-download
  :config
  (add-hook 'dired-mode-hook 'org-download-enable))


; (Use-package org-pomodoro
;   :bind	("C-x p" . org-pomodoro)
;   :config
;   (setq org-agenda-clockreport-parameter-plist '(:fileskip0 t :link t :maxlevel 2 :formula "$5=($3+$4)*(60/25);t"))
;   (setq org-clock-sound t)
;;   )

; (use-package calfw
;   :ensure calfw-org
;   :init
;   (require 'calfw-org)
;   ;:config
;   ;(setq cfw:org-overwrite-default-keybinding t)
;   :bind
;   (:map org-mode-map
; 	("C-c v" . cfw:open-org-calendar)))

(provide 'init-org)
