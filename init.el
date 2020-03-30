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

(setq my-name "Ying Rui"
      my-email "Ying.Rui@outlook.com")

(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                           ("melpa" . "http://elpa.emacs-china.org/melpa/")))
(add-to-list 'load-path "~/.emacs.d/config")
(package-initialize)

(require 'use-package)
(require 'package-lists)
(require 'basic-config)
(require 'org-config)
(require 'package-configs.el)

(setq inferior-ess-r-program "R")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("669e02142a56f63861288cc585bee81643ded48a19e36bfdf02b66d745bcc626" "55c2069e99ea18e4751bd5331b245a2752a808e91e09ccec16eb25dadbe06354" default)))
 '(package-selected-packages
   (quote
    (elpy ess benchmark-init all-the-icons-dired neotree doom-themes seoul256-theme dired-sidebar zenburn-theme yasnippet-snippets use-package transient swiper spaceline smex rainbow-delimiters pyim projectile plantuml-mode pdf-tools org-bullets nyan-mode nord-theme markdown-mode macrostep latex-preview-pane git-commit flycheck flucui-themes dracula-theme company avy auctex atom-one-dark-theme 0blayout))))
 
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "dark slate gray")))))
(put 'set-goal-column 'disabled nil)
