;;; aider-backends.el --- Backend implementations for aider.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Backend dispatcher for aider.el

;;; Code:

(defgroup aider-backends nil
  "Backend customization for aider."
  :group 'aider)

(defcustom aider-backend 'comint
  "Backend to use for the aider process.
Options are 'comint (the default) or 'vterm. When set to 'vterm, aider will
launch a fully functional vterm buffer (with bracketed paste support) instead
of using a comint process."
  :type '(choice (const :tag "Comint" comint)
                 (const :tag "VTerm" vterm))
  :group 'aider-backends)


(require 'aider-backend-comint)
(when (and (eq aider-backend 'vterm) (not (featurep 'vterm)))
  (require 'aider-backend-vterm))


;; Backend dispatcher functions
(defun aider-run-aider-backend (program args buffer-name)
  "Run aider using the selected backend.
PROGRAM is the aider executable path, ARGS are command line arguments,
and BUFFER-NAME is the name for the aider buffer."
  (cond
   ((eq aider-backend 'vterm)
    (aider-run-aider-vterm program args buffer-name))
   (t
    (aider-run-aider-comint program args buffer-name))))

(defun aider--send-command-backend (buffer command &optional switch-to-buffer)
  "Send COMMAND to BUFFER using the appropriate backend.
If SWITCH-TO-BUFFER is non-nil, switch to the buffer after sending."
  (cond
   ((eq aider-backend 'vterm)
    (aider--send-command-vterm buffer command switch-to-buffer))
   (t
    (aider--send-command-comint buffer command switch-to-buffer))))

(provide 'aider-backends)

;;; aider-backends.el ends here
