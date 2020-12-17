;;; lsp-fortran.el --- Fortran support for lsp-mode

;; Copyright (C) 2018 Magnus Badel <magnus.leo93@gmail.com>

;; Author: Magnus Badel
;; Version: 0.1.0
;; Package-Requires: ((lsp-mode "3.0"))
;; Keywords: fortran, Fortran, language server
;; URL: https://github.com/MagB93/lsp-fortran

;;; Code:
(require 'lsp-mode)

(defcustom lsp-fortran-command "fortls"
  "Command to invoke the fortran lanuage server: https://github.com/hansec/fortran-language-server"
  :type 'string
  :group 'lsp-fortran
)

(defcustom lsp-fortran-flags
  '("--symbol_skip_mem" "--incremental_sync" "--autocomplete_no_prefix")
  "Extra arguments for the fortran-stdio language server"
  :group 'lsp-fortran
  :risky t
  :type '(repeat-string)
)

(defun lsp-fortran--get-root ()
  "Try to find the language server configuration, refer to https://github.com/hansec/fortran-language-server."
  (lsp-make-traverser #'(lambda (dir)
                          (directory-files
                          dir
                          nil
                          ".fortls")))
)

(defun lsp-fortran--ls-command ()
"Generate the language server startup command."
`(,lsp-fortran-command ,@lsp-fortran-flags)
)

(defconst lsp-fortran--handlers
  '(("window/setStatusBusy" .
     (lambda (w _p)))
    ("window/setStatusReady" .
     (lambda(w _p)))))

(defun lsp-fortran--initialize-client(client)
  "Notifify the client for a successful startup.
CLIENT is the passed variable by :initialize."
  (mapcar #'(lambda (p) (lsp-client-on-notification client (car p) (cdr p))) lsp-fortran--handlers)
  )

(lsp-define-stdio-client lsp-fortran "f90"
                          (lsp-fortran--get-root) nil
                          :command-fn #'lsp-fortran--ls-command
                          :initialize #'lsp-fortran--initialize-client)

(provide 'lsp-fortran)
;;; lsp-fortran.el ends here
