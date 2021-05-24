;;read my blog to find how to install prerequisites
(use-package mu4e
  :defer 2
  :load-path "/usr/local/share/emacs/site-lisp/mu/mu4e"
  :config
  ;;Emacs Mail setting
  (setq user-mail-address "ying.rui@outlook.com")
  (setq user-full-name "Rui Ying/应锐")

  ;; SMTP settings:
  (setq send-mail-function 'smtpmail-send-it)    ; should not be modified
  (setq smtpmail-smtp-user "ying.rui@outlook.com")
  (setq smtpmail-smtp-server "smtp.office365.com") ; host running SMTP server
  (setq smtpmail-smtp-service 587)               ; SMTP service port number
  (setq smtpmail-stream-type 'starttls)          ; type of SMTP connections to use
  (setq smtpmail-use-gnutls t)
  (setq mu4e-get-mail-command "mbsync -a")

  ;; Mail folders:
  (setq mu4e-maildir "~/Maildir")
  (setq mu4e-drafts-folder "/Drafts")
  (setq mu4e-sent-folder   "/Sent")
  (setq mu4e-trash-folder  "/Deleted Items")
  (setq mu4e-attachments-dir "~/Downloads")

  ;; Further customization:
  (setq mu4e-html2text-command "w3m -T text/html" ; how to hanfle html-formatted emails
	mu4e-update-interval 300                  ; seconds between each mail retrieval
	mu4e-headers-auto-update t                ; avoid to type `g' to update
	mu4e-view-show-images t                   ; show images in the view buffer
	mu4e-compose-signature-auto-include nil   ; I don't want a message signature
	mu4e-use-fancy-chars t)                   ; allow fancy icons for mail threads

  ;;Others
  (setq mu4e-main-buffer-hide-personal-addresses t)
  )

(provide 'init-mu4e)
;;; init-mu4e ends here
