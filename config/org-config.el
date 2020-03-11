;; 默认开启标题缩进
(setq org-startup-indented t)

(add-to-list 'file-coding-system-alist
	     '("\\.org" . utf-8))

;;设置项目符号，使用org-bullets包
(require 'org-bullets)
(add-hook 'org-mode-hook 'org-bullets-mode)

;; 默认开启自动转换特殊字符及数学公式模式
(add-hook 'org-mode-hook 'org-toggle-pretty-entities)

;;设置关键词
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS"  "|" "DONE" "CANCELED"))
      )

;; 网上找的todo keywords 背景色
(setf org-todo-keyword-faces '(("TODO" . (:foreground "white" :background "red"   :weight bold))
                                ("HAND" . (:foreground "white" :background "#2E8B57"  :weight bold))
                                ("DONE" . (:foreground "white" :background "#3498DB" :weight bold))))

;; 设置插入源代码
(add-hook 'org-mode-hook '(lambda ()
			    (local-set-key (kbd "C-c C-e")
                                           'org-edit-src-code)
			    ))

;;设置ditaa流程图mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((ditaa . t))) ; this line activates ditaa
;;设置jar path
(setq org-ditaa-jar-path "~/.emacs.d/ditaa.jar");;使用方法C-c C-c编译, C-c C-x C-v预览

;;使用plantuml
(org-babel-do-load-languages
 'org-babel-load-languages
 '((plantuml . t)))
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
;;设置包路径
(setq plantuml-jar-path "~/plantuml.jar")
(setq org-plantuml-jar-path "~/plantuml.jar")
;;在org-mode里用plantuml
;(add-to-list
;org-src-lang-modes '("plantuml" . plantuml))

;;设置agenda-view快捷键
(global-set-key (kbd "C-c a") 'org-agenda)
(org-agenda-to-appt t);;事件提醒



(setq org-src-fontify-natively t) ;;高亮

(provide 'org-config)
