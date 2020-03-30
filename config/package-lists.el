(setq package-lists
      '(all-the-icons-dired ;;dire mode icon
	use-package
	dired-sidebar ;;dire mode sidebar
	yasnippet-snippets ;;provide code snippets
	transient
	swiper
	counsel
	smex
	company
	nyan-mode ;;small cat
	flycheck
	avy
	magit
	projectile
	benchmark-init
	spaceline ;;mode line
	rainbow-delimiters ;;brackets with different colors
	popwin
	org-bullets
	markdown-mode))

;;set theme list, then you can change theme in ~/config/basic-config.el
(setq theme-lists
      '(doom-themes
      seoul256-theme
      kaolin-themes
      zenburn-theme
      nord-theme
      flucui-themes
      dracula-theme))

;;-----------------------------------
;; Auto download package
;;-----------------------------------
;; fetch the list of packages available
;; package list is set in variable file
(unless package-archive-contents
  (package-refresh-contents))
;; install the missing packages
(dolist (package package-lists)
  (unless (package-installed-p package)
    (package-install package)))
;; install missing themes
(dolist (package theme-lists)
  (unless (package-installed-p package)
    (package-install package)))
(setq use-package-always-ensure t)

(provide 'package-lists)
;;; package-lists.el ends here

