(defun open-init-file()
  "Open the init.el file under .emacs.d directory."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(global-set-key (kbd "<f2>") 'open-init-file)
(global-set-key (kbd "C-x k") 'kill-this-buffer)

(defun select-current-line ()
  "Select the current line."
  (interactive)
  (move-beginning-of-line 1)
  (push-mark nil nil t)
  ;;(forward-line 1)
  (end-of-line 1))
(global-set-key (kbd "C-l") 'select-current-line)

;; Set meta command and mark set keybind for Mac OS
;; If you are using a external Windows keyboard, remeber to choose
;; USB keyboard in Preference -> Keyboard -> modify keyboard -> select keyboard
(if (eq system-type 'darwin)
    (setq mac-command-modifier 'meta))
;;Just found for Emacs-macport version you can swipe between buffer
;;by using two fingers (cool!)

;;; In case you can't use C-SPEC to do markset, change it to C-j
;; (global-set-key (kbd "C-j") 'set-mark-command)
;;C-x C-x -> set mark and go back
;;C-x h to select all

;; terminal in Emacs
(global-set-key (kbd "C-x t") 'shell)
;; keybinding in shell-mode
;; Tips: you can use M-r to search in shell history
;; History references like '!' (reference), ‘!!’ (last cmd) and ‘^’ (substituion, e.g., ^a^b) are supported
;; If you don't know the history reference, use C-c C-l to list all (will work for most comint buffers)
(with-eval-after-load 'shell
  (define-key shell-mode-map (kbd "<up>") 'comint-previous-input)
  (define-key shell-mode-map (kbd "C-p") 'comint-previous-input)
  (define-key shell-mode-map (kbd "<down>") 'comint-next-input)
  (define-key shell-mode-map (kbd "C-n") 'comint-next-input)
  (define-key shell-mode-map (kbd "C-l") 'comint-clear-buffer)
  (define-key shell-mode-map (kbd "SPC") 'comint-magic-space)) ;;magically expand history reference, <TAB> also works

;;a human-friendly keymap comparing to the default ones
;;alternatives: vimish-fold, Origami
(add-hook 'prog-mode-hook #'hs-minor-mode)
(global-set-key (kbd "<f5>") 'hs-toggle-hiding)

(provide 'init-keymap)
;;; init-keymap.el ends here
