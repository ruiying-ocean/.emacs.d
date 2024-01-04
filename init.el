;;; init.el -*- lexical-binding: t; -*-

;; Author: Rui Ying
;; Email: ying.rui@outlook.com


;; Customize when to check package modification (much much faster)
(setq-default straight-check-for-modifications '(check-on-save find-Hfwhen-checking))

;; Cause straight.el to cache the autoloads of all used packages in a single
;; file on disk thus reduce IO operations
(setq-default straight-cache-autoloads t)

;; Initialise PACKAGE MANAGER: straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; set up use-package
(if (version< emacs-version "29.1")
    (straight-use-package 'use-package)
  (require 'use-package))


(setq-default straight-use-package-by-default t)
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
(require 'programming)
(require 'theme)
(require 'markup)
(when (display-graphic-p)
  (require 'extra))
