(setq my-name "Ying Rui")
(setq my-email "Ying.Rui@outlook.com")
(add-to-list 'load-path "~/.emacs.d/config")
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(setq package-lists
      '(use-package
      all-the-icons-dired
      neotree
      dired-sidebar
      yasnippet-snippets
      transient
      swiper
      spaceline
      smex
      rainbow-delimiters
      projectile
      plantuml-mode
      pdf-tools 
      org-bullets
      nyan-mode
      markdown-mode
      macrostep
      latex-preview-pane
      git-commit
      flycheck
      company
      avy
      auctex
      0blayout))

(setq theme-lists
      '(doom-themes
      seoul256-theme
      kaolin-themes
      zenburn-theme
      atom-one-dark-theme
      nord-theme
      flucui-themes
      dracula-theme))
