;;; early-init.el --- early bird  -*- no-byte-compile: t -*-

;;gabage collection setting (temporarily)
(setq gc-cons-threshold most-positive-fixnum)

;;frame setting
(setq default-frame-alist
      (append (list
	       '(min-height . 1)  '(height     . 45)
	       '(min-width  . 1) '(width      . 81)
               '(vertical-scroll-bars . nil)
               '(internal-border-width . 20)
               '(left-fringe    . 0)
               '(right-fringe   . 0)
               '(tool-bar-lines . 0)
               '(menu-bar-lines . 0))))

(setq initial-major-mode 'fundamental-mode)

;;avoid oudated byte-compile-warnings
(setq load-prefer-newer t)

(setq byte-compile-warnings '(cl-functions))
