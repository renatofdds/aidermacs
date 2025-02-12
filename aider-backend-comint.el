;;; aider-backend-comint.el --- Comint backend for aider.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Comint backend implementation for aider.el

;;; Code:

(require 'comint)

(defcustom aider-comint-multiline-newline-key "S-<return>"
  "Key binding for `comint-accumulate' in Aidermacs buffers.
This allows for multi-line input without sending the command."
  :type 'string
  :group 'aider)

(defun aider-run-aider-comint (program args buffer-name)
  "Create a comint-based buffer and run aider PROGRAM with ARGS in BUFFER-NAME."
  (let ((comint-terminfo-terminal "dumb"))
    (unless (comint-check-proc buffer-name)
      (apply 'make-comint-in-buffer "aider" buffer-name program nil args)
      (with-current-buffer buffer-name
        (comint-mode)
        (setq-local comint-input-sender 'aider-input-sender)
        (setq aider--font-lock-buffer
              (get-buffer-create (concat " *aider-fontify" buffer-name)))
        (add-hook 'kill-buffer-hook #'aider-kill-buffer nil t)
        (add-hook 'comint-output-filter-functions #'aider-fontify-blocks 100 t)
        (let ((local-map (make-sparse-keymap)))
          (set-keymap-parent local-map comint-mode-map)
          (define-key local-map (kbd aider-comint-multiline-newline-key) #'comint-accumulate)
          (use-local-map local-map))
        (font-lock-add-keywords nil aider-font-lock-keywords t)))))

(defun aider--send-command-comint (buffer command &optional switch-to-buffer)
  "Send COMMAND to the aider comint BUFFER.
If SWITCH-TO-BUFFER is non-nil, switch to the buffer after sending."
  (with-current-buffer buffer
    (let ((process (get-buffer-process buffer))
          (inhibit-read-only t))
      (goto-char (process-mark process))
      (insert (propertize command
                         'face 'aider-command-text
                         'font-lock-face 'aider-command-text
                         'rear-nonsticky t))
      (set-marker (process-mark process) (point))
      (comint-send-string process (concat command "\n")))))

(provide 'aider-backend-comint)

;;; aider-backend-comint.el ends here
