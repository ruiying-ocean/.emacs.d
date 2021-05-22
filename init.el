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
(require 'editor-configs)
(require 'prog-configs)
(require 'init-ui)
(require 'init-theme)
(require 'init-binding)
(require 'init-recentf)
(require 'init-company)
;(require 'init-tex)
;(require 'init-org)
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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("bc4c89a7b91cfbd3e28b2a8e9e6750079a985237b960384f158515d32c7f0490" "a3bdcbd7c991abd07e48ad32f71e6219d55694056c0c15b4144f370175273d16" "95d0ed21bb0e919be7687a25ad59a1c2c8df78cbe98c9e369d44e65bfd65b167" "75b8719c741c6d7afa290e0bb394d809f0cc62045b93e1d66cd646907f8e6d43" "e3c64e88fec56f86b49dcdc5a831e96782baf14b09397d4057156b17062a8848" "461e9e0d69636be8b5347a030f14b16c996c60a89e48c33f48bde51c32da3248" "801a567c87755fe65d0484cb2bded31a4c5bb24fd1fe0ed11e6c02254017acb2" "dbade2e946597b9cda3e61978b5fcc14fa3afa2d3c4391d477bdaeff8f5638c5" "c0a0c2f40c110b5b212eb4f2dad6ac9cac07eb70380631151fa75556b0100063" "21055a064d6d673f666baaed35a69519841134829982cbbb76960575f43424db" "2c613514f52fb56d34d00cc074fe6b5f4769b4b7f0cc12d22787808addcef12c" "fee4e306d9070a55dce4d8e9d92d28bd9efe92625d2ba9d4d654fc9cd8113b7f" default))
 '(org-export-backends '(ascii beamer html icalendar latex md))
 '(package-selected-packages
   '(mu4e magic-latex-buffer flycheck-inline fycheck-inline org-download dumb-jump dump-jump sublime-themes emacs-color-themes tao-theme mixed-pitch org-super-agenda rainbow-mode ace-window esup flyspell-correct-popup flyspell-correct-avy-menu flyspell-correct-ivy spacemacs-themes dracula-theme zenburn-theme color-theme-sanityinc-solarized color-theme-sanityinc-tomorrow ayu-theme dashboard org-fancy-priorities org-superstar org-superstar-mode visual-fill-column lsp-mode lsp-python-ms ccls company-lsp ivy-rich lsp-ui company-posframe ivy-posframe mini-modeline company-quickhelp company-anaconda anaconda-mode lab-themes haskell-emacs-base company-box company-tabnine spaceline amx neotree base16-theme org-pomodoro org calfw-org ess grip-mode pydoc avy counsel ivy latex-preview-pane auctex smex markdown-mode yasnippet-snippets doom-modeline all-the-icons rainbow-delimiters flycheck projectile which-key nyan-mode popwin exec-path-from-shell magit transient company doom-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
