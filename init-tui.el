(menu-bar-mode -1)

(global-set-key (kbd "C-x ,") 'beginning-of-buffer)
(global-set-key (kbd "C-x .") 'end-of-buffer)

;; ------------------------------------------------------------
;;;; Enable mouse operation in terminal emacs
;; specifies the mode where <BS> or <BACKSPACE> is <DEL>
(normal-erase-is-backspace-mode 0)
(xterm-mouse-mode 1)
(global-set-key (kbd "<mouse-4>") 'scroll-down-line)
(global-set-key (kbd "<mouse-5>") 'scroll-up-line)

;;-----------Dired setting/replacement-------------
(require 'dired)
(setq dired-listing-switches "-alFhv")
(setq dired-dwim-target t)
(setq dired-dwim-target t)

(defun hide-dired-mode-info ()
  "show less information in dired buffers"
  (dired-hide-details-mode 1))
(add-hook 'dired-mode-hook 'hide-dired-mode-info)

(require 'vertico)
(vertico-mode t)


