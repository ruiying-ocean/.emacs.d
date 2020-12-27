
(use-package company
  :config
  (setq company-idle-delay 0.5)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode 1)
  )

(use-package company-tabnine ;TabNine uses ML to provide suggestion
  :after 'company-mode 
  'company-tabnine-mode 
  :config (add-to-list 'company-backends #'company-tabnine)
  (setq company-idle-delay 0)
  (setq company-show-numbers t)
  )

;; (use-package company-quickhelp
;;   :config
;;   (add-hook 'company-quickhelp-mode-hook 'python-mode)
;;   ;(company-quickhelp-mode 1) ;;globally true
;;   (eval-after-load 'company
;;     '(define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin)))

;; (use-package company-posframe ;;similar function as company-quickhelp
;;   :config
;;   (company-posframe-mode 1)
;;   )
(use-package company-box
  :hook (company-mode . company-box-mode))
(provide 'init-company)
