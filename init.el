;; This is Emacs init file of Rui Ying
;; The license can be found in root directory

;; Code:
(require 'package)
(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
;; Load path
;; Optimize: Force "lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))
(advice-add #'package-initialize :after #'update-load-path)
(update-load-path)

(setq gc-cons-percentage 0.6)
(setq read-process-output-max (* 1024 1024))

(when (< emacs-major-version 27)
  (package-initialize))

(unless (package-installed-p 'use-package)  
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

;;load modules
(require 'init-editor)
(require 'init-core)
(require 'init-binding)
(require 'init-recentf)
(require 'init-ui)
(require 'init-theme)
(require 'init-tex)
(require 'init-org)
(require 'init-md)
(require 'init-eglot)
(require 'init-mu4e)
(require 'init-pdf)

(defun my-cleanup-gc ()
  "Clean up gc."
  (setq gc-cons-threshold  (* 128 1024 1024)) ; 64M
  (setq gc-cons-percentage 0.3) ; original value
  (garbage-collect))
(run-with-idle-timer 4 nil #'my-cleanup-gc)

(provide 'init)
;;; init.el ends here
