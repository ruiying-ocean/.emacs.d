;;Editor coding system setting
(fset 'yes-or-no-p 'y-or-n-p)
(progn
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8))

;;No more backup files~
(setq-default make-backup-files nil)
(setq ring-bell-function 'ignore)

;;Font Setting
(setq inhibit-compacting-font-caches t)

;;Varialble/fixed pictch font setting
(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "New York" :height 200))))
 '(fixed-pitch ((t ( :family "Monaco" :height 160)))))

(custom-theme-set-faces
 'user
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

;;unicode font
(when (member "Symbola" (font-family-list))
 (set-fontset-font "fontset-default" nil
 (font-spec :size 25 :name "Symbola")))

(when (member "Symbola" (font-family-list))
 (set-fontset-font t 'unicode "Symbola" nil 'prepend))

;;flyspell setting
(add-hook 'text-mode-hook 'flyspell-mode)

;; (setq-default ispell-program-name "/usr/local/bin/aspell") ;;depends on aspell in the path
;; (setq ispell-local-dictionary "en_GB")
;; (setq ispell-extra-args '("--sug-mode=fast"))

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

(use-package flyspell-correct-avy-menu
  :after flyspell-correct)

(use-package flyspell-correct-ivy
  :after flyspell-correct)

 (use-package flyspell-correct-popup
   :after flyspell-correct)

(provide 'init-editor)
;;; init-editor.el ends here
