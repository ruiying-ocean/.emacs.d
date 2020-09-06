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
      my-email "Ying.Rui@outlook.com")

(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                           ("melpa" . "http://elpa.emacs-china.org/melpa/")))
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))
(package-initialize)

(require 'use-package)
(require 'basic-config)
(require 'package-configs.el)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("bc4c89a7b91cfbd3e28b2a8e9e6750079a985237b960384f158515d32c7f0490" "b8929cff63ffc759e436b0f0575d15a8ad7658932f4b2c99415f3dde09b32e97" "c968804189e0fc963c641f5c9ad64bca431d41af2fb7e1d01a2a6666376f819c" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "76c5b2592c62f6b48923c00f97f74bcb7ddb741618283bdb2be35f3c0e1030e3" "b3bcf1b12ef2a7606c7697d71b934ca0bdd495d52f901e73ce008c4c9825a3aa" "fede08d0f23fc0612a8354e0cf800c9ecae47ec8f32c5f29da841fe090dfc450" "196df8815910c1a3422b5f7c1f45a72edfa851f6a1d672b7b727d9551bb7c7ba" "aea30125ef2e48831f46695418677b9d676c3babf43959c8e978c0ad672a7329" "16dd114a84d0aeccc5ad6fd64752a11ea2e841e3853234f19dc02a7b91f5d661" "0cd56f8cd78d12fc6ead32915e1c4963ba2039890700458c13e12038ec40f6f5" "7f6d4aebcc44c264a64e714c3d9d1e903284305fd7e319e7cb73345a9994f5ef" "2296db63b1de14e65390d0ded8e2b5df4b9e4186f3251af56807026542a58201" "cf9f20cab61999609e47b969f6d7a89c148e16f94ae3f2f127fecfc27dc878d3" "ed573618e4c25fa441f12cbbb786fb56d918f216ae4a895ca1c74f34a19cfe67" "054e929c1df4293dd68f99effc595f5f7eb64ff3c064c4cfaad186cd450796db" "a7928e99b48819aac3203355cbffac9b825df50d2b3347ceeec1e7f6b592c647" "b69323309e5839676409607f91c69da2bf913914321c995f63960c3887224848" "a70b47c87e9b0940f6fece46656200acbfbc55e129f03178de8f50934ac89f58" "e1ecb0536abec692b5a5e845067d75273fe36f24d01210bf0aa5842f2a7e029f" "1526aeed166165811eefd9a6f9176061ec3d121ba39500af2048073bea80911e" "aad7998b78e7901bd7eaa9585410c53c773925607bd1918ad4a40c8ec6010e41" "99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" "669e02142a56f63861288cc585bee81643ded48a19e36bfdf02b66d745bcc626" "55c2069e99ea18e4751bd5331b245a2752a808e91e09ccec16eb25dadbe06354" default)))
 '(org-agenda-files
   (quote
    ("~/.emacs.d/org/inbox.org" "~/.emacs.d/org/agenda.org")))
 '(package-selected-packages
   (quote
    (exec-path-from-shell cnfonts doom-themes autopair smartparens org-pomodoro color-theme-sanityinc-solarized base16-theme org-mode calfw-org calfw ess-r-mode org-caldav grip-mode company-jedi elpy ess benchmark-init all-the-icons-dired neotree seoul256-theme dired-sidebar zenburn-theme yasnippet-snippets use-package transient swiper spaceline smex rainbow-delimiters pyim projectile plantuml-mode pdf-tools org-bullets nyan-mode nord-theme markdown-mode macrostep latex-preview-pane git-commit flycheck dracula-theme company avy auctex atom-one-dark-theme 0blayout))))
 
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
