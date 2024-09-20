;; necessary user interface settings

;; lazy-load default theme
(setq custom-safe-themes t)

;;Transprancy setting
(set-frame-parameter (selected-frame) 'alpha '(97 100))
(add-to-list 'default-frame-alist '(alpha 97 100))

;; Set font and auto-fullscreen in daemon-mode, put after init-ui.el
(if (daemonp)
    (add-hook 'after-make-frame-functions
	      (lambda (frame)
		(with-selected-frame frame
		  (self/setup-font)
		  (auto-max-frame))))
  (add-hook 'after-init-hook 'self/setup-font))


;;Font Setting
(defvar myfont "RobotoMono Nerd Font")

(defun self/setup-font ()
  "Set English and CJK font for Emacs."
  (interactive)
  ;; English font
  (if (display-graphic-p)
      (progn
	;; English font
	(set-face-attribute 'default nil :font (format "%s:pixelsize=%d" myfont 15))
	;; CJK font
	(dolist (charset '(kana han symbol cjk-misc bopomofo))
	  (set-fontset-font (frame-parameter nil 'font)
			    charset
			    (font-spec :family "Noto Serif SC" :size 16))))))

(setq inhibit-compacting-font-caches t)

;;Varialble/fixed pictch font setting, essential for org-mode
(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "LXGW WenKai" :height 160))))
 '(fixed-pitch ((t ( :family " Inconsolata" :height 160)))))

(provide 'theme)
