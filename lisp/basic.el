;; this file includes the most fundamental customisations
;; e.g., coding system

(when (eq system-type 'darwin)
  (defvar brew-parent-dir "/opt/homebrew/")
  (defvar brew-bin-dir (expand-file-name "bin/" brew-parent-dir))
  (defvar emacs-app-dir "/opt/homebrew/Cellar/emacs-mac/emacs-29.1-mac-10.0/")
  (defvar conda-dir "~/miniforge3/envs/workspace/"))


;; Avoid matching file name with regrex list during startup
(let ((file-name-handler-alist nil)) "~/.emacs.d/init.el")

;; Custom file
(setq-default custom-file (concat user-emacs-directory "extra-lisp/custom.el"))
(load custom-file :noerror)

;;Coding system
;; In some old machines, you might need specify these in .bashrc
;; export LAGNUAGE=en_US.UTF-8
;; export LANG=en_US.UTF-8
;; export LC_ALL=en_US.UTF-8
;; export LC_CTYPE=en_US.UTF-8

(defvar default-buffer-file-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8 . utf-8))
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; abbreviaiont of yes/no
(if (> emacs-major-version 27)
    (setq use-short-answers t)
  (fset 'yes-or-no-p 'y-or-n-p))

;;No more backup files~
(setq-default make-backup-files nil)

;;No more strange ring bell
(setq ring-bell-function 'ignore)

;; Don't ask for killing
(setq confirm-kill-processes nil)

;; Delete selection
(delete-selection-mode t)

;; right key
(context-menu-mode 1)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time


;;; KEYBINDING

;; Set meta command for Mac OS
;; If you are using a external Windows keyboard, remeber to choose
;; USB keyboard in Preference -> Keyboard -> modify keyboard -> select keyboard
(if (eq system-type 'darwin)
    (setq mac-command-modifier 'meta))

(global-set-key (kbd "C-M-;") 'comment-box)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-x ,") 'beginning-of-buffer)
(global-set-key (kbd "C-x .") 'end-of-buffer)

;; globally go to previous position; "C-u C-SPC" to do same locally
(global-set-key (kbd "C-c C-SPC") 'pop-global-mark)
;; repeat command
(global-set-key (kbd "<f4>") #'repeat)

(defun select-current-line ()
  "Select the current line."
  (interactive)
  (move-beginning-of-line 1)
  (push-mark nil nil t)
  (end-of-line 1))

(global-set-key (kbd "C-l") 'select-current-line)

(defvar self/init-file-path (expand-file-name (concat user-emacs-directory "init.el")))
(defun self/open-init-file()
  "Open my init.el."
  (interactive)
  (find-file self/init-file-path))
(global-set-key (kbd "<f2>") 'self/open-init-file)

(when (eq system-type 'darwin)
  (progn
    (setq dired-use-ls-dired nil) ;;to avoid error "ls does not support --dired" in MacOS
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
    (add-to-list 'default-frame-alist '(ns-appearance . light))))

(defun auto-max-frame ()
  "Maxize/full screen the frame according to the OS type."
  (interactive)
  (if (eq system-type 'darwin)
      (toggle-frame-maximized)
    (toggle-frame-fullscreen)))
(auto-max-frame)

;;Cursor
(setq-default cursor-type 'bar)
(blink-cursor-mode 0)

;; keep .emacs.d clean
(use-package no-littering)

;;This package reads proper environment variable in MacOS GUI version
;;To speed up this package, (1) separate configuration into
;;non-interactive (.zshenv) and interactive (.zshrc) part;
;;(2) set explicit path in .zshenv (which is what we will use, you should
;;put your PATH variable like /usr/local/bin/python3.9 in this file)
;;Find out more in https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-arguments nil) ;;read non-interactive shell config
  (exec-path-from-shell-initialize))

;; Emacs 28: Hide commands in M-x which do not work in the current mode.
;; Vertico commands are hidden in normal buffers.
(setq read-extended-command-predicate
      #'command-completion-default-include-p)

;; restart emacs
(use-package restart-emacs
  :commands (restart-emacs))

;; pop up window management
(use-package popwin
  :hook
  (after-init . popwin-mode))

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)

;; adjust font size
(setq-default text-scale-mode-step 1.1)

;; draws the underline at the same height as the font's descent line
(setq x-underline-at-descent-line t)

(setq frame-inhibit-implied-resize nil)

;; provide a command to ensure the third-party packages are installed
(use-package use-package-ensure-system-package
  :after exec-path-from-shell) ;;extend use-package, put after exec-path-from-shell


;; Use emacs daemon, put following lines to shell config file
;; alias emacs=/path_to_miniconda3/bin/emacs
;; alias emacsclient=/path_to_miniconda/bin/emacsclient
;; alias ed="emacs --daemon"
;; alias ec="emacsclient -c"
;; alias eq="emacsclient -e '(save-buffers-kill-emacs)'"
;; If you want more fuzzy cmd:
;; alias emcas=emacs
;; alias emasc=emacs
;; alias enacs=emacs

(provide 'basic)
