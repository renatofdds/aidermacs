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

(defvar aidermacs--current-output nil
  "Accumulator for current output being captured.")

(defun aidermacs-get-output-history (&optional limit)
  "Get the output history, optionally limited to LIMIT entries.
Returns a list of (timestamp . output-text) pairs, most recent first."
  (let ((history aidermacs--output-history))
    (if limit
        (seq-take history limit)
      history)))

(defun aidermacs-get-last-output ()
  "Get the most recent output from aidermacs."
  (car (aidermacs-get-output-history 1)))

(defun aidermacs-clear-output-history ()
  "Clear the output history."
  (interactive)
  (setq aidermacs--output-history nil))


(defun aidermacs-show-output-history ()
  "Display the AI output history in a new buffer."
  (interactive)
  (let ((buf (get-buffer-create "*aidermacs-history*"))
        (history aidermacs--output-history)) ; Get history from current buffer
    (with-current-buffer buf
      (erase-buffer)
      (display-line-numbers-mode 1)
      (dolist (entry history) ; Use passed history
        (let ((timestamp (format-time-string "%Y-%m-%d %H:%M:%S" (car entry)))
              (output (cdr entry)))
          (insert (format "=== %s ===\n%s\n\n" timestamp output))))
      (goto-char (point-min)))
    (display-buffer buf)))

(defun aidermacs-copy-last-output ()
  "Copy the most recent AI output to the kill ring."
  (interactive)
  (if-let ((last-output (cdr (aidermacs-get-last-output))))
      (progn
        (kill-new last-output)
        (message "Copied last AI output to kill ring"))
    (message "No AI output available")))

(defun aidermacs--store-output (output)
  "Store OUTPUT in the history with timestamp."
  (setq aidermacs--current-output output)
  (push (cons (current-time) output) aidermacs--output-history)
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

(defun aidermacs--send-command-backend (buffer command &optional switch-to-buffer)
  "Send COMMAND to BUFFER using the appropriate backend.
If SWITCH-TO-BUFFER is non-nil, switch to the buffer after sending."
  (cond
   ((eq aidermacs-backend 'vterm)
    (aidermacs--send-command-vterm buffer command switch-to-buffer))
   (t
    (aidermacs--send-command-comint buffer command switch-to-buffer))))

(provide 'aidermacs-backends)

;;; aidermacs-backends.el ends here
