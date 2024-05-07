;;; early-init.el --- early bird  -*- no-byte-compile: t -*-

(setq gc-cons-threshold most-positive-fixnum)

;; Increase read max limit to 3 mb
(setq read-process-output-max (* 3 1024 1024))

(setq frame-inhibit-implied-resize t)

;;frame setting
(setq default-frame-alist
      (append (list
	       '(min-height . 1) '(height . 45)
	       '(min-width . 1) '(width . 81)
	       '(internal-border-width . 20)
	       '(left-fringe . 10)
	       '(right-fringe . 10)
	       '(vertical-scroll-bars . nil) ;; remove the scroll bar	
	       '(tool-bar-lines . 0)	     ;;remove the tool bar
	       '(menu-bar-lines . 0)	     ;;remove the menu bar
	       '(undecorated . t))))

(setq-default window-resize-pixelwise t)
(setq-default frame-resize-pixelwise t)

(setq initial-major-mode 'fundamental-mode)

;;avoid oudated byte-compile-warnings
(setq load-prefer-newer t)

(setq byte-compile-warnings '(cl-functions))

;; disable package.el if version is greater than 27.0
(when (version< emacs-version "27.0")
  (package-initialize)
  (setq package-enable-at-startup nil))

;;no more startup message/screen
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(setq read-process-output-max (* 1024 1024))
(setq process-adaptive-read-buffering nil)
