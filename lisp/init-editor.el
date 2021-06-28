;; ;;Editor coding system setting
(progn
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8))

;;We are lazy human
(fset 'yes-or-no-p 'y-or-n-p)

;;No more backup files~
(setq-default make-backup-files nil)

;;No more strange ring bell
(setq ring-bell-function 'ignore)

;;Font Setting
(setq inhibit-compacting-font-caches t)

;;Varialble/fixed pictch font setting, essential for org-mode
(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "Cormorant Garamond" :height 230))))
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

;;Unicode font setting
(when (member "Symbola" (font-family-list))
  (set-fontset-font "fontset-default" nil
		    (font-spec :size 20 :name "Symbola")))

(when (member "Symbola" (font-family-list))
 (set-fontset-font t 'unicode "Symbola" nil 'prepend))

;;flyspell setting
(add-hook 'text-mode-hook 'flyspell-mode)
(setq-default ispell-program-name "aspell") ;;depends on aspell in the path
(setq ispell-local-dictionary "en_GB")
(setq ispell-extra-args '("--sug-mode=fast" "--lang=en_GB" "--camel-case" "--run-together"))

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

;;Interface options
;; (use-package flyspell-correct-avy-menu
;;   :after flyspell-correct)

(use-package flyspell-correct-ivy
  :after flyspell-correct)

;; (use-package flyspell-correct-ido
;;   :after flyspell-correct)

;;  (use-package flyspell-correct-popup
;;    :after flyspell-correct)

;; ---KEYBINDINGS---
;; >>> OPTION1: evil (vim-like)
;; (use-package evil
;;   :hook
;;   (after-init . evil-mode)
;;   :config
;;   (setq evil-disable-insert-state-bindings t)
;;   )

;; >>> OPTION2: viper-mode (built-in vim-like)
;; (use-package viper
;;   :ensure nil
;;   :init (setq viper-mode t)
;;   )

;; >>> OPTION3: god-mode (remove prefix key)
;; (use-package god-mode
;;   :init
;;   (god-mode)
;;   )

(provide 'init-editor)
;;; init-editor.el ends here
