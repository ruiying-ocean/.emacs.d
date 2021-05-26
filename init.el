;; This is Emacs init file of Rui Ying
;; The license can be found in root directory

;; Code:
(require 'package)
(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
;; Load path
;; Optimize: Force "lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))
(advice-add #'package-initialize :after #'update-load-path)
(update-load-path)

(setq gc-cons-percentage 0.6)
(setq read-process-output-max (* 1024 1024))

(when (< emacs-major-version 27)
  (package-initialize))

(unless (package-installed-p 'use-package)  
  (package-refresh-contents)
  (package-install 'use-package))

;;recompile outdated .elc file
(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode)
)

(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

;;load modules
(require 'init-editor)
(require 'init-core)
(require 'init-binding)
(require 'init-recentf)
(require 'init-ui)
(require 'init-theme)
(require 'init-tex)
(require 'init-org)
(require 'init-md)
(require 'init-eglot)
(require 'init-mu4e)
(require 'init-pdf)

(defun my-cleanup-gc ()
  "Clean up gc."
  (setq gc-cons-threshold  (* 128 1024 1024)) ; 64M
  (setq gc-cons-percentage 0.3) ; original value
  (garbage-collect))
(run-with-idle-timer 4 nil #'my-cleanup-gc)

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(dashboard jupyter yasnippet-snippets which-key visual-fill-column use-package unicode-fonts treemacs-all-the-icons tramp toc-org tao-theme sublime-themes spacemacs-theme smex smartparens simple-httpd rg rainbow-mode rainbow-delimiters popwin poet-theme pdf-tools org-superstar org-super-agenda org-fancy-priorities org-download nyan-mode neotree mood-line mixed-pitch minimap mini-frame markdown-mode magit magic-latex-buffer latex-preview-pane ivy-yasnippet humanoid-themes highlight-symbol highlight-indent-guides helpful gruvbox-theme grip-mode flyspell-correct-popup flyspell-correct-ivy flyspell-correct-avy-menu flycheck-inline exec-path-from-shell esup ess ein eglot dumb-jump doom-themes doom-modeline counsel-tramp counsel-projectile company-tabnine company-box color-theme-sanityinc-tomorrow cnfonts centaur-tabs benchmark-init base16-theme auctex all-the-icons-ivy-rich all-the-icons-dired aggressive-indent ag affe)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(fixed-pitch ((t (:family "Monaco" :height 160))))
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
 '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
 '(variable-pitch ((t (:family "New York" :height 200)))))
