(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(global-set-key (kbd "<f2>") 'open-init-file)
(global-set-key (kbd "C-x k") 'kill-this-buffer)

(defun select-current-line ()
  "Select the current line"
  (interactive)
  (move-beginning-of-line 1)
  (push-mark nil nil t)
  ;;(forward-line 1)
  (end-of-line 1)
  )
(global-set-key (kbd "C-x l") 'select-current-line)

;; Set meta command and mark set keybind for Mac OS
;; If you are using a external Windows keyboard, remeber to choose
;; USB keyboard in Preference -> Keyboard -> modify keyboard -> select keyboard
(if (eq system-type 'darwin)
    (setq mac-command-modifier 'meta))
(global-set-key (kbd "C-j") 'set-mark-command)
;;C-x C-x -> set mark and go back
;;C-x h to select all

;a human-friendly keymap comparing to the default ones
(add-hook 'prog-mode-hook #'hs-minor-mode)
(global-set-key (kbd "<f5>") 'hs-toggle-hiding)

(provide 'init-keymap)
