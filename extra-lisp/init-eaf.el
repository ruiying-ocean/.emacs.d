;;EAF, as its name -- Emacs application framework, 
;;allows better pdf view and many other GUI features on Linux platform.
;;However, you may not enable it if you already have good experience
;;in your daily work such as viewing pdf (especially on OS other than Linux)

;;Installation

;;then run M-x eaf-install-dependencies
(quelpa '(eaf :fetcher github
              :repo  "manateelazycat/emacs-application-framework"
              :files ("*")))

(use-package eaf
  :load-path "~/.emacs.d/site-lisp/emacs-application-framework" ; Set to "/usr/share/emacs/site-lisp/eaf" if installed from AUR
  :init
  (use-package epc :defer t :ensure t)
  (use-package ctable :defer t :ensure t)
  (use-package deferred :defer t :ensure t)
  (use-package s :defer t :ensure t)
  :custom
  (eaf-browser-continue-where-left-off t)
  :config
  (eaf-setq eaf-browser-enable-adblocker "true")
  (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  (eaf-bind-key take_photo "p" eaf-camera-keybinding)
  (eaf-bind-key nil "M-q" eaf-browser-keybinding)
)


;;https://gitlab.com/blak3mill3r/emacs-ludicrous-speed
;;to further improve the startup speed, use this in Linux
(provide 'init-eaf)
