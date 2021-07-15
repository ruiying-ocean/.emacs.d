;;; early-init.el --- early bird  -*- no-byte-compile: t -*-

;; Temporaily increase garbage collection threshold at startup
(setq gc-cons-percentage 0.6)
(setq gc-cons-threshold most-positive-fixnum)

;; Increase read max limit to 3 mb
(setq read-process-output-max (* 3 1024 1024))

;;frame setting
(setq default-frame-alist
      (append (list
	       '(min-height . 1)  '(height     . 45)
	       '(min-width  . 1) '(width      . 81)
               '(vertical-scroll-bars . nil)
               '(internal-border-width . 20)
               '(left-fringe    . 0)
               '(right-fringe   . 0)
               '(tool-bar-lines . 0) ;;remove the tool bar
               '(menu-bar-lines . 0) ;;remove the menu bar
	       '(undecorated . t)))) ;;remove the title bar

(setq initial-major-mode 'fundamental-mode)

;;avoid oudated byte-compile-warnings
(setq load-prefer-newer t)

(setq byte-compile-warnings '(cl-functions))

;; straight.el doesn't need package.el setting
(setq package-enable-at-startup nil)
