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

(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

(when (< emacs-major-version 27)
  (package-initialize))

(require 'use-package)
(require 'basic-config)
(require 'package-configs.el)
(require 'init-lsp.el)
(require 'init-python.el)
 
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-indent-guides-method 'character)
 '(package-selected-packages
   '(lsp-mode lsp-python-ms ccls company-lsp ivy-rich lsp-ui company-posframe ivy-posframe mini-modeline company-quickhelp company-anaconda anaconda-mode lab-themes haskell-emacs-base company-box company-tabnine spaceline amx neotree base16-theme org-pomodoro org org-bullets calfw-org ess grip-mode pydoc avy counsel ivy latex-preview-pane auctex smex markdown-mode yasnippet-snippets doom-modeline all-the-icons rainbow-delimiters flycheck projectile which-key nyan-mode popwin exec-path-from-shell magit transient company benchmark-init use-package doom-themes)))
