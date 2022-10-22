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
	       ;; '(undecorated . 0) ;; remove the title bar
	       )))

(setq-default window-resize-pixelwise t)
(setq-default frame-resize-pixelwise t)

(setq initial-major-mode 'fundamental-mode)

(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
(set-face-background 'fringe (face-attribute 'default :background))



;;avoid oudated byte-compile-warnings
(setq load-prefer-newer t)

(setq byte-compile-warnings '(cl-functions))

;; disable package.el
(setq package-enable-at-startup nil)
