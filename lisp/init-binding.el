(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(global-set-key (kbd "<f2>") 'open-init-file)
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;set meta command
(if (eq system-type 'darwin)
    (setq mac-command-modifier 'meta))
(if (eq system-type 'windows-nt)
    (global-set-key (kbd "C-j") 'set-mark-command))

;a human-friendly keymap comparing to the default ones
(add-hook 'prog-mode-hook #'hs-minor-mode)
(global-set-key (kbd "<f5>") 'hs-toggle-hiding)

(provide 'init-binding)
