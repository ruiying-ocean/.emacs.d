;;; init.el -*- lexical-binding: t; -*-

;; Author: Rui Ying
;; Email: ying.rui@outlook.com

;; set up use-package
(if (version< emacs-version "29.1")
    (package-install 'use-package)
  (require 'use-package))

(setq use-package-enable-imenu-support t)

;; Emacs Native Compilation Feature support
(when (and (fboundp 'native-comp-available-p)
	   (native-comp-available-p))
  (progn
    (setq-default native-comp-async-report-warnings-errors nil)
    (setq-default comp-deferred-compilation t)
    (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))
    (setq package-native-compile t)))

;; Update user load path
;; Optimize: Force "lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path`"
  (dolist (dir '("lisp" "elpa"))
    (push (expand-file-name dir user-emacs-directory) load-path)))
(advice-add #'package-initialize :after #'update-load-path)
(update-load-path)

(require 'basic)
(require 'editor)
(require 'theme)
(require 'programming)

(when (display-graphic-p)
  (require 'markup))

(defun my-cleanup-gc ()
  "Clean up gc."
  (setq gc-cons-threshold  67108864)
  (garbage-collect))
(run-with-idle-timer 4 nil #'my-cleanup-gc)
