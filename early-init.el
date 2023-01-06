;;; early-init.el --- early bird  -*- no-byte-compile: t -*-

;; Temporaily increase garbage collection threshold at startup
(setq gc-cons-percentage 0.6)
(setq gc-cons-threshold most-positive-fixnum)

;; Increase read max limit to 3 mb
(setq read-process-output-max (* 3 1024 1024))

(setq frame-inhibit-implied-resize t)

;;frame setting
(setq default-frame-alist
      (append (list
	       '(min-height . 1)  '(height     . 45)
	       '(min-width  . 1) '(width      . 81)
               '(internal-border-width . 20)
               '(left-fringe    . 10)
               '(right-fringe   . 10)
               '(vertical-scroll-bars . nil) ;; remove the scroll bar	       
               '(tool-bar-lines . 0) ;;remove the tool bar
               '(menu-bar-lines . 0) ;;remove the menu bar
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

;; fancy splash
(defun self/show-welcome-buffer ()
  "Show *Welcome* buffer."
  (with-current-buffer (get-buffer-create "*Welcome*")
    (setq truncate-lines t)
    (let* ((buffer-read-only)
           (image-path "~/.emacs.d/fancy-splash/world.png")
           (image (create-image image-path))
           (size (image-size image))
           (height (cdr size))
           (width (car size))
           (top-margin (floor (/ (- (window-height) height) 2)))
           (left-margin (floor (/ (- (window-width) width) 2)))
           (title "[q] to quit buffer, [\\ f] to open file"))
      (erase-buffer)
      (setq mode-line-format nil)
      (goto-char (point-min))
      (insert (make-string top-margin ?\n ))
      (insert (make-string left-margin ?\ ))
      (insert-image image)
      (insert "\n\n\n")
      (insert (make-string (floor (/ (- (window-width) (string-width title)) 2)) ?\ ))
      (insert title))
    (setq cursor-type nil)
    (read-only-mode +1)
    (switch-to-buffer (current-buffer))
    (local-set-key (kbd "q") 'kill-this-buffer)))

;;no more startup message/screen
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(when (< (length command-line-args) 2)
  (add-hook 'emacs-startup-hook (lambda ()
                                  (when (display-graphic-p)
                                    (self/show-welcome-buffer)))))
