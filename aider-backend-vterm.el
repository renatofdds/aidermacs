;;; aider-backend-vterm.el --- VTerm backend for aider.el -*- lexical-binding: t; -*-

;;; Commentary:
;; VTerm backend implementation for aider.el

;;; Code:

(require 'vterm nil t)

(defun aider-run-aider-vterm (program args buffer-name)
  "Create a vterm-based buffer and run aider PROGRAM with ARGS in BUFFER-NAME."
  (unless (require 'vterm nil t)
    (error "vterm package is not available"))
  (unless (get-buffer buffer-name)
    (let ((cmd (concat program " " (mapconcat 'identity args " "))))
      (setq vterm-shell cmd)
      (setq vterm-buffer-name buffer-name)
      (vterm)
      (with-current-buffer buffer-name
        (setq-local aider-backend 'vterm)
        (aider-minor-mode 1)))))

(defun aider--send-command-vterm (buffer command &optional switch-to-buffer)
  "Send COMMAND to the aider vterm BUFFER.
If SWITCH-TO-BUFFER is non-nil, switch to the buffer after sending."
  (with-current-buffer buffer
    (vterm-send-string command)
    (vterm-send-return)))

(provide 'aider-backend-vterm)

;;; aider-backend-vterm.el ends here
