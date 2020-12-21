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

(setq my-name "Rui Ying"
      my-email "ying.rui@outlook.com")

(setq package-archives '(("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

;; (setq package-archives
;;       '(("melpa" . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/melpa/")
;;         ("org"   . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/org/")
;;         ("gnu"   . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/gnu/")))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(when (< emacs-major-version 27)
  (package-initialize))

(require 'use-package)
(require 'basic-config)
(require 'package-configs)
(require 'init-ui)
(require 'init-theme)
(require 'init-binding)
(require 'init-recentf)
(require 'init-company)
(require 'init-python)
(require 'init-eglot)
(require 'init-org)
(require 'init-md)
(require 'init-tex)
;;(require 'init-lsp.el)

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
   '("0c3b1358ea01895e56d1c0193f72559449462e5952bded28c81a8e09b53f103f" "b8929cff63ffc759e436b0f0575d15a8ad7658932f4b2c99415f3dde09b32e97" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" default))
 '(highlight-indent-guides-method 'character)
 '(package-selected-packages
   '(dashboard org-fancy-priorities org-superstar org-superstar-mode visual-fill-column spacemacs-theme lsp-mode lsp-python-ms ccls company-lsp ivy-rich lsp-ui company-posframe ivy-posframe mini-modeline company-quickhelp company-anaconda anaconda-mode lab-themes haskell-emacs-base company-box company-tabnine spaceline amx neotree base16-theme org-pomodoro org calfw-org ess grip-mode pydoc avy counsel ivy latex-preview-pane auctex smex markdown-mode yasnippet-snippets doom-modeline all-the-icons rainbow-delimiters flycheck projectile which-key nyan-mode popwin exec-path-from-shell magit transient company benchmark-init use-package doom-themes)))
