(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(global-set-key (kbd "<f2>") 'open-init-file)
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;set meta command and mark set keybind
(if (eq system-type 'darwin)
    (setq mac-command-modifier 'meta))
(global-set-key (kbd "C-j") 'set-mark-command)
;;C-x C-x -> set mark and go back
;;C-x h to select all

;a human-friendly keymap comparing to the default ones
(add-hook 'prog-mode-hook #'hs-minor-mode)
(global-set-key (kbd "<f5>") 'hs-toggle-hiding)

(provide 'init-binding)
