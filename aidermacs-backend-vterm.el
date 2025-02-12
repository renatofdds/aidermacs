;;; aidermacs-backend-vterm.el --- VTerm backend for aidermacs.el -*- lexical-binding: t; -*-

;;; Commentary:
;; VTerm backend implementation for aidermacs.el

;;; Code:

(require 'vterm nil t)

(defun aidermacs-run-aidermacs-vterm (program args buffer-name)
  "Create a vterm-based buffer and run aidermacs PROGRAM with ARGS in BUFFER-NAME."
  (unless (require 'vterm nil t)
    (error "vterm package is not available"))
  (unless (get-buffer buffer-name)
    (let* ((mode (if (eq (frame-parameter nil 'background-mode) 'dark)
                     "--dark-mode"
                   "--light-mode"))
           (cmd (mapconcat 'identity (append (list program mode) args) " ")))
      (setq vterm-shell cmd)
      (setq vterm-buffer-name buffer-name)
      (vterm)
      (with-current-buffer buffer-name
        (setq-local aidermacs-backend 'vterm)
        (aidermacs-minor-mode 1)))))

(defun aidermacs--send-command-vterm (buffer command &optional switch-to-buffer)
  "Send COMMAND to the aidermacs vterm BUFFER.
If SWITCH-TO-BUFFER is non-nil, switch to the buffer after sending."
  (with-current-buffer buffer
    (vterm-send-string command)
    (vterm-send-return)))

(provide 'aidermacs-backend-vterm)

;;; aidermacs-backend-vterm.el ends here
