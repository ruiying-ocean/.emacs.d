;;; pdfkit-reader.el --- Minimal native PDF reader for macOS -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Rui Ying
;; Copyright (C) 2026 Chao Huang

;; Author: Rui Ying <r.ying@uea.ac.uk>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: files, multimedia, pdf
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; pdfkit-reader embeds one plain macOS PDFKit view in an Emacs window.
;; It intentionally has no tabs, toolbar, browser, annotations, or session
;; manager.  Emacs owns the keybindings; PDFKit owns display and trackpad
;; scrolling.
;;
;; The native-view embedding approach is derived in part from Appine:
;; https://github.com/chaoswork/appine

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup pdfkit-reader nil
  "A small native PDF reader for macOS."
  :group 'files
  :prefix "pdfkit-reader-")

(defcustom pdfkit-reader-auto-build-module t
  "When non-nil, build the native module when it is missing or stale."
  :type 'boolean
  :group 'pdfkit-reader)

(defcustom pdfkit-reader-initial-scale 'fit-width
  "Initial scale used when opening a document.
The value may be `fit-width' or `fit-page'."
  :type '(choice (const :tag "Fit width" fit-width)
                 (const :tag "Fit page" fit-page))
  :group 'pdfkit-reader)

(defconst pdfkit-reader--directory
  (file-name-directory
   (or load-file-name
       (locate-library "pdfkit-reader")
       (error "Cannot determine pdfkit-reader package directory"))))

(defconst pdfkit-reader--module-file
  (expand-file-name "pdfkit-reader-module.dylib"
                    pdfkit-reader--directory))

(defconst pdfkit-reader--module-sources
  (mapcar (lambda (name)
            (expand-file-name name pdfkit-reader--directory))
          '("pdfkit-reader-module.m" "Makefile")))

(defvar pdfkit-reader--last-module-error nil)
(defvar pdfkit-reader--buffers nil)
(defvar pdfkit-reader--next-id 0)
(defvar pdfkit-reader--hooks-installed nil)
(defvar doc-view-doc-type)

(defvar-local pdfkit-reader--id nil)
(defvar-local pdfkit-reader--opened nil)
(defvar-local pdfkit-reader--visible nil)
(defvar-local pdfkit-reader--last-window nil)
(defvar-local pdfkit-reader--last-rect nil)
(defvar-local pdfkit-reader--fallback-pending nil)

(declare-function pdfkit-reader-native-open "pdfkit-reader-module"
                  (id path x y width height))
(declare-function pdfkit-reader-native-update "pdfkit-reader-module"
                  (id x y width height))
(declare-function pdfkit-reader-native-set-visible "pdfkit-reader-module"
                  (id visible))
(declare-function pdfkit-reader-native-action "pdfkit-reader-module"
                  (id action))
(declare-function pdfkit-reader-native-goto-page "pdfkit-reader-module"
                  (id page-index))
(declare-function pdfkit-reader-native-page-info "pdfkit-reader-module"
                  (id))
(declare-function pdfkit-reader-native-close "pdfkit-reader-module"
                  (id))
(declare-function pdfkit-reader-native-close-all "pdfkit-reader-module" ())
(declare-function doc-view-set-doc-type "doc-view" ())

(defun pdfkit-reader--module-stale-p ()
  "Return non-nil when the native module needs to be built."
  (or (not (file-exists-p pdfkit-reader--module-file))
      (cl-some
       (lambda (source)
         (and (file-exists-p source)
              (file-newer-than-file-p source
                                      pdfkit-reader--module-file)))
       pdfkit-reader--module-sources)))

;;;###autoload
(defun pdfkit-reader-build-module ()
  "Build the native PDFKit module.
When a previously loaded module is rebuilt, restart Emacs before using
the new binary."
  (interactive)
  (unless (eq system-type 'darwin)
    (user-error "This reader only supports macOS"))
  (unless (executable-find "make")
    (user-error "Cannot build pdfkit-reader: `make' was not found"))
  (let ((default-directory pdfkit-reader--directory)
        (buffer (get-buffer-create "*pdfkit-reader build*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (let ((status (call-process "make" nil buffer t)))
      (unless (and (integerp status) (zerop status)
                   (file-exists-p pdfkit-reader--module-file))
        (display-buffer buffer)
        (error "Native pdfkit-reader build failed; see %s"
               (buffer-name buffer))))
    (when (called-interactively-p 'interactive)
      (message "Built %s%s"
               pdfkit-reader--module-file
               (if (featurep 'pdfkit-reader-module)
                   "; restart Emacs to load the new binary"
                 "")))
    pdfkit-reader--module-file))

(defun pdfkit-reader--ensure-module ()
  "Build and load the native module, returning non-nil on success."
  (or
   (featurep 'pdfkit-reader-module)
   (condition-case error-data
       (progn
         (unless (eq system-type 'darwin)
           (error "This reader only supports macOS"))
         (unless (fboundp 'module-load)
           (error "This Emacs was built without dynamic module support"))
         (when (and pdfkit-reader-auto-build-module
                    (pdfkit-reader--module-stale-p))
           (pdfkit-reader-build-module))
         (unless (file-exists-p pdfkit-reader--module-file)
           (error "Native module is missing; run M-x pdfkit-reader-build-module"))
         (module-load pdfkit-reader--module-file)
         (unless (featurep 'pdfkit-reader-module)
           (error "Native module loaded without providing its feature"))
         (setq pdfkit-reader--last-module-error nil)
         t)
     (error
      (setq pdfkit-reader--last-module-error
            (error-message-string error-data))
      (message "pdfkit-reader: %s" pdfkit-reader--last-module-error)
      nil))))

(defun pdfkit-reader--window-rect (window)
  "Return WINDOW's native (X Y WIDTH HEIGHT) rectangle."
  (let* ((edges (window-inside-pixel-edges window))
         (left (nth 0 edges))
         (top (nth 1 edges))
         (right (nth 2 edges))
         (bottom (nth 3 edges))
         (frame-height (frame-pixel-height (window-frame window))))
    (mapcar #'truncate
            (list left
                  (- frame-height bottom)
                  (- right left)
                  (- bottom top)))))

(defun pdfkit-reader--display-window (buffer)
  "Return the `selected-frame' window where BUFFER should be rendered."
  (let ((selected (selected-window)))
    (if (eq (window-buffer selected) buffer)
        selected
      (cl-find-if
       (lambda (window)
         (and (window-live-p window)
              (eq (window-frame window) (selected-frame))))
       (get-buffer-window-list buffer nil t)))))

(defun pdfkit-reader--queue-fallback (buffer reason)
  "Arrange for BUFFER to fall back to DocView because of REASON."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (unless pdfkit-reader--fallback-pending
        (setq pdfkit-reader--fallback-pending t)
        (run-at-time 0 nil #'pdfkit-reader--fallback-now
                     buffer reason)))))

(defun pdfkit-reader--fallback-now (buffer reason)
  "Switch BUFFER to DocView and report REASON."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (eq major-mode 'pdfkit-reader-mode)
        (setq pdfkit-reader--fallback-pending nil)
        (pdfkit-reader--cleanup)
        (when (and buffer-file-name
                   (file-readable-p buffer-file-name))
          (let ((inhibit-read-only t)
                (coding-system-for-read 'no-conversion))
            (erase-buffer)
            (insert-file-contents-literally buffer-file-name)
            (set-buffer-modified-p nil)
            (set-visited-file-modtime)))
        (require 'doc-view)
        (condition-case doc-view-error
            (progn
              (doc-view-set-doc-type)
              (if (doc-view-mode-p doc-view-doc-type)
                  (progn
                    ;; Call DocView directly.  `doc-view-mode-maybe'
                    ;; restores the previous major mode when a converter is
                    ;; unavailable, which would otherwise bounce straight
                    ;; back into pdfkit-reader.
                    (doc-view-mode)
                    (message "pdfkit-reader: %s; using DocView"
                             reason))
                (fundamental-mode)
                (message
                 "pdfkit-reader: %s; no DocView renderer is available"
                 reason)))
          (error
           (fundamental-mode)
           (message "pdfkit-reader: %s; DocView also failed: %s"
                    reason
                    (error-message-string doc-view-error))))))))

(defun pdfkit-reader--sync-buffer (buffer)
  "Synchronize BUFFER's native view with its Emacs window."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (eq major-mode 'pdfkit-reader-mode)
                 pdfkit-reader--id
                 (not pdfkit-reader--fallback-pending))
        (let ((window (pdfkit-reader--display-window buffer)))
          (if (not window)
              (when (and pdfkit-reader--opened
                         pdfkit-reader--visible)
                (ignore-errors
                  (pdfkit-reader-native-set-visible
                   pdfkit-reader--id nil))
                (setq pdfkit-reader--visible nil))
            (let ((rect (pdfkit-reader--window-rect window)))
              (condition-case error-data
                  (progn
                    (unless pdfkit-reader--opened
                      (unless
                          (apply #'pdfkit-reader-native-open
                                 pdfkit-reader--id
                                 (file-truename buffer-file-name)
                                 rect)
                        (error "PDFKit could not open %s"
                               (file-name-nondirectory
                                buffer-file-name)))
                      (setq pdfkit-reader--opened t)
                      (pdfkit-reader-native-action
                       pdfkit-reader--id
                       (if (eq pdfkit-reader-initial-scale 'fit-page)
                           "fit-page"
                         "fit-width")))
                    (when (or (not (eq window
                                       pdfkit-reader--last-window))
                              (not (equal rect
                                          pdfkit-reader--last-rect)))
                      (unless
                          (apply #'pdfkit-reader-native-update
                                 pdfkit-reader--id rect)
                        (error "Could not attach PDFKit to the Emacs window"))
                      (setq pdfkit-reader--last-window window
                            pdfkit-reader--last-rect rect))
                    (unless pdfkit-reader--visible
                      (pdfkit-reader-native-set-visible
                       pdfkit-reader--id t)
                      (setq pdfkit-reader--visible t)))
                (error
                 (pdfkit-reader--queue-fallback
                  buffer (error-message-string error-data)))))))))))

(defun pdfkit-reader--sync-all (&rest _ignored)
  "Synchronize every live pdfkit-reader buffer."
  (setq pdfkit-reader--buffers
        (cl-delete-if-not #'buffer-live-p pdfkit-reader--buffers))
  (dolist (buffer (copy-sequence pdfkit-reader--buffers))
    (pdfkit-reader--sync-buffer buffer)))

(defun pdfkit-reader--install-hooks ()
  "Install global hooks that keep native views aligned."
  (unless pdfkit-reader--hooks-installed
    (setq pdfkit-reader--hooks-installed t)
    (add-hook 'window-buffer-change-functions
              #'pdfkit-reader--sync-all)
    (add-hook 'window-size-change-functions
              #'pdfkit-reader--sync-all)
    (add-hook 'window-configuration-change-hook
              #'pdfkit-reader--sync-all)
    (add-hook 'window-state-change-hook
              #'pdfkit-reader--sync-all)
    (add-function :after after-focus-change-function
                  #'pdfkit-reader--sync-all)
    (when (boundp 'window-selection-change-functions)
      (add-hook 'window-selection-change-functions
                #'pdfkit-reader--sync-all))))

(defun pdfkit-reader--cleanup ()
  "Close the native view owned by the current buffer."
  (when pdfkit-reader--id
    (when (featurep 'pdfkit-reader-module)
      (ignore-errors
        (pdfkit-reader-native-close pdfkit-reader--id)))
    (setq pdfkit-reader--id nil
          pdfkit-reader--opened nil
          pdfkit-reader--visible nil
          pdfkit-reader--last-window nil
          pdfkit-reader--last-rect nil))
  (setq pdfkit-reader--buffers
        (delq (current-buffer) pdfkit-reader--buffers)))

(defun pdfkit-reader--perform (action &optional report-page)
  "Perform native ACTION, optionally REPORT-PAGE afterwards."
  (unless (and pdfkit-reader--opened pdfkit-reader--id)
    (pdfkit-reader--sync-buffer (current-buffer)))
  (unless (and pdfkit-reader--opened pdfkit-reader--id
               (pdfkit-reader-native-action
                pdfkit-reader--id action))
    (user-error "The native PDF view is not available"))
  (when report-page
    (pdfkit-reader-page-info)))

(defun pdfkit-reader-page-info ()
  "Report the current page and page count."
  (interactive)
  (let ((info (and pdfkit-reader--opened
                   pdfkit-reader--id
                   (pdfkit-reader-native-page-info
                    pdfkit-reader--id))))
    (if info
        (progn
          (message "Page %d of %d" (car info) (cdr info))
          info)
      (when (called-interactively-p 'interactive)
        (user-error "No page information is available")))))

(defun pdfkit-reader-next-page ()
  "Go to the next page."
  (interactive)
  (pdfkit-reader--perform "next-page" t))

(defun pdfkit-reader-previous-page ()
  "Go to the previous page."
  (interactive)
  (pdfkit-reader--perform "previous-page" t))

(defun pdfkit-reader-first-page ()
  "Go to the first page."
  (interactive)
  (pdfkit-reader--perform "first-page" t))

(defun pdfkit-reader-last-page ()
  "Go to the last page."
  (interactive)
  (pdfkit-reader--perform "last-page" t))

(defun pdfkit-reader-scroll-page-down ()
  "Scroll down by one screen."
  (interactive)
  (pdfkit-reader--perform "scroll-page-down"))

(defun pdfkit-reader-scroll-page-up ()
  "Scroll up by one screen."
  (interactive)
  (pdfkit-reader--perform "scroll-page-up"))

(defun pdfkit-reader-scroll-line-down ()
  "Scroll down by one line."
  (interactive)
  (pdfkit-reader--perform "scroll-line-down"))

(defun pdfkit-reader-scroll-line-up ()
  "Scroll up by one line."
  (interactive)
  (pdfkit-reader--perform "scroll-line-up"))

(defun pdfkit-reader-zoom-in ()
  "Increase the PDF scale."
  (interactive)
  (pdfkit-reader--perform "zoom-in"))

(defun pdfkit-reader-zoom-out ()
  "Decrease the PDF scale."
  (interactive)
  (pdfkit-reader--perform "zoom-out"))

(defun pdfkit-reader-fit-width ()
  "Fit the current page to the window width."
  (interactive)
  (pdfkit-reader--perform "fit-width"))

(defun pdfkit-reader-fit-page ()
  "Fit the current page inside the window."
  (interactive)
  (pdfkit-reader--perform "fit-page"))

(defun pdfkit-reader-goto-page (page)
  "Go to one-based PAGE."
  (interactive
   (let* ((info (pdfkit-reader-page-info))
          (last-page (or (cdr info) 1)))
     (list (read-number
            (format "Page (1-%d): " last-page)
            (or (car info) 1)))))
  (unless (and pdfkit-reader--opened pdfkit-reader--id)
    (pdfkit-reader--sync-buffer (current-buffer)))
  (unless (pdfkit-reader-native-goto-page
           pdfkit-reader--id (1- page))
    (user-error "Page must be between 1 and the last page"))
  (pdfkit-reader-page-info))

(defun pdfkit-reader-revert (_ignore-auto _noconfirm)
  "Reload the current PDF from disk."
  (unless (and buffer-file-name
               (file-readable-p buffer-file-name))
    (user-error "This PDF is no longer readable"))
  (when (and pdfkit-reader--id
             (featurep 'pdfkit-reader-module))
    (ignore-errors
      (pdfkit-reader-native-close pdfkit-reader--id)))
  (setq pdfkit-reader--opened nil
        pdfkit-reader--visible nil
        pdfkit-reader--last-window nil
        pdfkit-reader--last-rect nil)
  (set-visited-file-modtime)
  (pdfkit-reader--sync-buffer (current-buffer))
  (message "Reloaded %s"
           (file-name-nondirectory buffer-file-name)))

;;;###autoload
(defun pdfkit-reader-open-in-doc-view ()
  "Open the current PDF with Emacs's built-in DocView."
  (interactive)
  (pdfkit-reader--fallback-now
   (current-buffer) "DocView requested"))

(defun pdfkit-reader-quit-window ()
  "Hide the native view, then quit the current Emacs window."
  (interactive)
  (when (and pdfkit-reader--opened pdfkit-reader--id)
    (ignore-errors
      (pdfkit-reader-native-set-visible pdfkit-reader--id nil))
    (setq pdfkit-reader--visible nil))
  (quit-window))

(defvar-keymap pdfkit-reader-mode-map
  :doc "Keymap for `pdfkit-reader-mode'."
  "n" #'pdfkit-reader-next-page
  "p" #'pdfkit-reader-previous-page
  "<right>" #'pdfkit-reader-next-page
  "<left>" #'pdfkit-reader-previous-page
  "SPC" #'pdfkit-reader-scroll-page-down
  "S-SPC" #'pdfkit-reader-scroll-page-up
  "DEL" #'pdfkit-reader-scroll-page-up
  "<next>" #'pdfkit-reader-scroll-page-down
  "<prior>" #'pdfkit-reader-scroll-page-up
  "C-n" #'pdfkit-reader-scroll-line-down
  "C-p" #'pdfkit-reader-scroll-line-up
  "<down>" #'pdfkit-reader-scroll-line-down
  "<up>" #'pdfkit-reader-scroll-line-up
  "<" #'pdfkit-reader-first-page
  ">" #'pdfkit-reader-last-page
  "g" #'pdfkit-reader-goto-page
  "M-g g" #'pdfkit-reader-goto-page
  "+" #'pdfkit-reader-zoom-in
  "=" #'pdfkit-reader-zoom-in
  "-" #'pdfkit-reader-zoom-out
  "0" #'pdfkit-reader-fit-page
  "w" #'pdfkit-reader-fit-width
  "f" #'pdfkit-reader-fit-page
  "r" #'revert-buffer
  "q" #'pdfkit-reader-quit-window
  "Q" #'kill-current-buffer)

;;;###autoload
(define-derived-mode pdfkit-reader-mode special-mode "PDFKit"
  "Read a PDF in a minimal native macOS PDFKit view."
  (if (not (pdfkit-reader--ensure-module))
      (pdfkit-reader--queue-fallback
       (current-buffer)
       (or pdfkit-reader--last-module-error
           "the native module is unavailable"))
    (unless (and buffer-file-name
                 (file-readable-p buffer-file-name))
      (user-error "A readable local PDF file is required"))
    (setq-local pdfkit-reader--id
                (format "pdfkit-reader-%d"
                        (cl-incf pdfkit-reader--next-id)))
    (setq-local revert-buffer-function #'pdfkit-reader-revert)
    (setq-local cursor-type nil)
    (setq-local truncate-lines t)
    (buffer-disable-undo)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert
       (propertize
        (concat "\n  Native PDFKit view\n\n"
                "  n/p     next/previous page\n"
                "  SPC/DEL scroll down/up\n"
                "  +/-     zoom\n"
                "  w/0     fit width/page\n"
                "  g       go to page\n"
                "  q       quit window\n")
        'face 'shadow))
      (set-buffer-modified-p nil))
    (cl-pushnew (current-buffer) pdfkit-reader--buffers)
    (add-hook 'kill-buffer-hook #'pdfkit-reader--cleanup nil t)
    (add-hook 'change-major-mode-hook
              #'pdfkit-reader--cleanup nil t)
    (pdfkit-reader--install-hooks)
    (run-at-time 0 nil #'pdfkit-reader--sync-buffer
                 (current-buffer))))

(defun pdfkit-reader-unload-function ()
  "Close native views and remove global hooks before unloading."
  (when (featurep 'pdfkit-reader-module)
    (ignore-errors
      (pdfkit-reader-native-close-all)))
  (remove-hook 'window-buffer-change-functions
               #'pdfkit-reader--sync-all)
  (remove-hook 'window-size-change-functions
               #'pdfkit-reader--sync-all)
  (remove-hook 'window-configuration-change-hook
               #'pdfkit-reader--sync-all)
  (remove-hook 'window-state-change-hook
               #'pdfkit-reader--sync-all)
  (remove-function after-focus-change-function
                   #'pdfkit-reader--sync-all)
  (when (boundp 'window-selection-change-functions)
    (remove-hook 'window-selection-change-functions
                 #'pdfkit-reader--sync-all))
  (setq pdfkit-reader--buffers nil
        pdfkit-reader--hooks-installed nil)
  nil)

(provide 'pdfkit-reader)

;;; pdfkit-reader.el ends here
