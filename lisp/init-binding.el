(defun open-config-file()
  "A simple customized function from Zilongshanren."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(global-set-key (kbd "<f2>") 'open-config-file)
(global-set-key (kbd "C-x k") 'kill-this-buffer)

(if (eq system-type 'darwin)
    (setq mac-command-modifier 'meta))
(if (eq system-type 'windows-nt)
    (global-set-key (kbd "C-j") 'set-mark-command))

(provide 'init-binding)
