;;; aidermacs-backends.el --- Backend implementations for aidermacs.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Backend dispatcher for aidermacs.el

;;; Code:

(require 'aidermacs-backend-comint)
(when (require 'vterm nil 'noerror)
  (require 'aidermacs-backend-vterm))

(defgroup aidermacs-backends nil
  "Backend customization for aidermacs."
  :group 'aidermacs)

(defcustom aidermacs-backend 'comint
  "Backend to use for the aidermacs process.
Options are 'comint (the default) or 'vterm. When set to 'vterm, aidermacs will
launch a fully functional vterm buffer (with bracketed paste support) instead
of using a comint process."
  :type '(choice (const :tag "Comint" comint)
                 (const :tag "VTerm" vterm))
  :group 'aidermacs-backends)

;; Core output management functionality
(defgroup aidermacs-output nil
  "Output handling for aidermacs."
  :group 'aidermacs)

(defcustom aidermacs-output-limit 10
  "Maximum number of output entries to keep in history."
  :type 'integer
  :group 'aidermacs-output)

(defvar aidermacs--output-history nil
  "List to store aidermacs output history.
Each entry is a cons cell (timestamp . output-text).")

(defvar aidermacs--last-command nil
  "Store the last command sent to aidermacs.")

(defvar aidermacs--current-output ""
  "Accumulator for current output being captured as a string.")

(defun aidermacs-get-output-history (&optional limit)
  "Get the output history, optionally limited to LIMIT entries.
Returns a list of (timestamp . output-text) pairs, most recent first."
  (let ((history aidermacs--output-history))
    (if limit
        (seq-take history limit)
      history)))

(defun aidermacs-clear-output-history ()
  "Clear the output history."
  (interactive)
  (setq aidermacs--output-history nil))

(defun aidermacs--store-output (output)
  "Store OUTPUT string in the history with timestamp."
  (setq aidermacs--current-output (substring-no-properties output))
  (push (cons (current-time) (substring-no-properties output)) aidermacs--output-history)
  (when (> (length aidermacs--output-history) aidermacs-output-limit)
    (setq aidermacs--output-history
          (seq-take aidermacs--output-history aidermacs-output-limit))))

;; Backend dispatcher functions
(defun aidermacs-run-aidermacs-backend (program args buffer-name)
  "Run aidermacs using the selected backend.
PROGRAM is the aidermacs executable path, ARGS are command line arguments,
and BUFFER-NAME is the name for the aidermacs buffer."
  (cond
   ((eq aidermacs-backend 'vterm)
    (aidermacs-run-aidermacs-vterm program args buffer-name))
   (t
    (aidermacs-run-aidermacs-comint program args buffer-name))))

(defun aidermacs--send-command-backend (buffer command)
  "Send COMMAND to BUFFER using the appropriate backend."
  (cond
   ((eq aidermacs-backend 'vterm)
    (aidermacs--send-command-vterm buffer command))
   (t
    (aidermacs--send-command-comint buffer command))))

(provide 'aidermacs-backends)

;;; aidermacs-backends.el ends here
