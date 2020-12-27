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

(setq package-archives '(("gnu"   . "http://elpa.gnu.org/packages/")
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
(require 'init-org)
(require 'init-md)
(require 'init-tex)
(require 'init-python) ;;use this on windows, eglot in linux instead
;;(require 'init-eglot) ;;conflict with elpy and some company packages
;;(require 'init-lsp)

(defun my-cleanup-gc ()
  "Clean up gc."
  (setq gc-cons-threshold  (* 128 1024 1024)) ; 64M
  (setq gc-cons-percentage 0.3) ; original value
  (garbage-collect))

(run-with-idle-timer 4 nil #'my-cleanup-gc)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:family "Fira Code" :height 120))))
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
 '(variable-pitch ((t (:family "ETBembo" :height 140 :weight thin)))))

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("2dff5f0b44a9e6c8644b2159414af72261e38686072e063aa66ee98a2faecf0e" "21055a064d6d673f666baaed35a69519841134829982cbbb76960575f43424db" "e6df46d5085fde0ad56a46ef69ebb388193080cc9819e2d6024c9c6e27388ba9" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "2c613514f52fb56d34d00cc074fe6b5f4769b4b7f0cc12d22787808addcef12c" "3325e2c49c8cc81a8cc94b0d57f1975e6562858db5de840b03338529c64f58d1" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" default))
 '(highlight-indent-guides-method 'character)
 '(org-export-backends '(ascii beamer html icalendar latex md))
 '(package-selected-packages
   '(esup flyspell-correct-popup flyspell-correct-avy-menu flyspell-correct-ivy spacemacs-themes dracula-theme zenburn-theme color-theme-sanityinc-solarized color-theme-sanityinc-tomorrow ayu-theme dashboard org-fancy-priorities org-superstar org-superstar-mode visual-fill-column spacemacs-theme lsp-mode lsp-python-ms ccls company-lsp ivy-rich lsp-ui company-posframe ivy-posframe mini-modeline company-quickhelp company-anaconda anaconda-mode lab-themes haskell-emacs-base company-box company-tabnine spaceline amx neotree base16-theme org-pomodoro org calfw-org ess grip-mode pydoc avy counsel ivy latex-preview-pane auctex smex markdown-mode yasnippet-snippets doom-modeline all-the-icons rainbow-delimiters flycheck projectile which-key nyan-mode popwin exec-path-from-shell magit transient company doom-themes)))
