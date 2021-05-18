;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;; This is Emacs init file of Rui Ying.

;;; Code:
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
(setq gc-cons-threshold most-positive-fixnum)
(setq read-process-output-max (* 1024 1024))

(when (< emacs-major-version 27)
  (package-initialize))

(unless (package-installed-p 'use-package)  
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'basic-config)
(require 'package-configs)
(require 'init-ui)
(require 'init-theme)
(require 'init-binding)
(require 'init-recentf)
(require 'init-company)
;(require 'init-org)
(require 'init-md)
(require 'init-eglot)

(defun my-cleanup-gc ()
  "Clean up gc."
  (setq gc-cons-threshold  (* 128 1024 1024)) ; 64M
  (setq gc-cons-percentage 0.3) ; original value
  (garbage-collect))
(run-with-idle-timer 4 nil #'my-cleanup-gc)

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("c0a0c2f40c110b5b212eb4f2dad6ac9cac07eb70380631151fa75556b0100063" "21055a064d6d673f666baaed35a69519841134829982cbbb76960575f43424db" "2c613514f52fb56d34d00cc074fe6b5f4769b4b7f0cc12d22787808addcef12c" "fee4e306d9070a55dce4d8e9d92d28bd9efe92625d2ba9d4d654fc9cd8113b7f" default))
 '(org-export-backends '(ascii beamer html icalendar latex md))
 '(package-selected-packages
   '(magic-latex-buffer flycheck-inline fycheck-inline org-download dumb-jump dump-jump sublime-themes emacs-color-themes tao-theme mixed-pitch org-super-agenda rainbow-mode ace-window esup flyspell-correct-popup flyspell-correct-avy-menu flyspell-correct-ivy spacemacs-themes dracula-theme zenburn-theme color-theme-sanityinc-solarized color-theme-sanityinc-tomorrow ayu-theme dashboard org-fancy-priorities org-superstar org-superstar-mode visual-fill-column lsp-mode lsp-python-ms ccls company-lsp ivy-rich lsp-ui company-posframe ivy-posframe mini-modeline company-quickhelp company-anaconda anaconda-mode lab-themes haskell-emacs-base company-box company-tabnine spaceline amx neotree base16-theme org-pomodoro org calfw-org ess grip-mode pydoc avy counsel ivy latex-preview-pane auctex smex markdown-mode yasnippet-snippets doom-modeline all-the-icons rainbow-delimiters flycheck projectile which-key nyan-mode popwin exec-path-from-shell magit transient company doom-themes)))
