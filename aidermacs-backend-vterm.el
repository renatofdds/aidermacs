;;; aidermacs-backend-vterm.el --- VTerm backend for aidermacs.el -*- lexical-binding: t; -*-

;;; Commentary:
;; VTerm backend implementation for aidermacs.el

;;; Code:

(require 'vterm nil 'noerror)

(defun aidermacs--vterm-output-advice (orig-fun &rest args)
  "Capture output before and after executing `vterm-send-return'.
This advice records the current prompt position as START-POINT,
calls ORIG-FUN (with ARGS) and then waits until the expected finish
sequence appears.  The expected finish sequence is defined as the
substring from the newline before `(vterm--get-prompt-point)` to the
newline after `(vterm--get-prompt-point)`, matching a regex of `\\n>.*`
or `\\ndiff>.*`.

Once that is detected, the output from START-POINT up to the beginning
of the finish sequence is captured and stored via `aidermacs--store-output`.

This is a covoluted HACK of capturing aider output until someone comes up with a better idea."
  (when (and (bound-and-true-p aidermacs-minor-mode)
             (eq major-mode 'vterm-mode))
    (let* ((start-point (vterm--get-prompt-point))
           (proc (get-buffer-process (current-buffer)))
           (expected "\n[^[:space:]]*>[[:space:]].*\n"))
      ;; Save the original process filter.
      (let ((orig-filter (process-filter proc)))
        ;; Set up our temporary filter.
        (set-process-filter
         proc
         (lambda (proc string)
           ;; Call the original filter first.
           (funcall orig-filter proc string)
           ;; Then check for our finish sequence.
           (let ((buffer (process-buffer proc)))
             (with-current-buffer buffer
               (let* ((prompt-point (vterm--get-prompt-point))
                      (seq-start (or (save-excursion
                                       (goto-char prompt-point)
                                       (search-backward "\n" nil t))
                                     (point-min)))
                      (seq-end (or (save-excursion
                                     (goto-char prompt-point)
                                     (search-forward "\n" nil t))
                                   (point-max)))
                      (finish-seq (buffer-substring-no-properties seq-start seq-end)))
                 (when (and (string-match-p expected finish-seq)
                            (< start-point prompt-point))
                   ;; Capture the output from the original start-point up to
                   ;; the beginning of the finish sequence.
                   (let ((output (buffer-substring-no-properties start-point seq-start)))
                     (aidermacs--store-output (string-trim output))
                     ;; Restore the original filter.
                     (set-process-filter proc orig-filter))))))))))
    (apply orig-fun args)))

(defun aidermacs-run-aidermacs-vterm (program args buffer-name)
  "Create a vterm-based buffer and run aidermacs PROGRAM with ARGS in BUFFER-NAME.
PROGRAM is the command to run, ARGS is a list of arguments,
and BUFFER-NAME is the name of the vterm buffer."
  (unless (require 'vterm nil t)
    (error "vterm package is not available"))
  (unless (get-buffer buffer-name)
    (let* ((mode (if (eq (frame-parameter nil 'background-mode) 'dark)
                     "--dark-mode"
                   "--light-mode"))
           (cmd (mapconcat 'identity (append (list program mode) args) " "))
           (vterm-buffer-name-orig vterm-buffer-name)
           (vterm-shell-orig vterm-shell))
      ;; Temporarily set globals so that the new buffer uses our values.
      (setq vterm-buffer-name buffer-name)
      (setq vterm-shell cmd)
      (with-current-buffer (vterm-other-window)
        (aidermacs-minor-mode 1)
        (advice-add 'vterm-send-return :around #'aidermacs--vterm-output-advice))
      ;; Restore the original globals.
      (setq vterm-buffer-name vterm-buffer-name-orig)
      (setq vterm-shell vterm-shell-orig)))
  buffer-name)

(defun aidermacs--send-command-vterm (buffer command &optional switch-to-buffer)
  "Send COMMAND to the aidermacs vterm BUFFER.
If SWITCH-TO-BUFFER is non-nil, switch to BUFFER after sending the command."
  (with-current-buffer buffer
    ;; Store command before sending
    (setq aidermacs--last-command command
          aidermacs--current-output nil)
    (vterm-send-string command)
    (vterm-send-return)
    (when switch-to-buffer
      (switch-to-buffer buffer))))

(provide 'aidermacs-backend-vterm)

;;; aidermacs-backend-vterm.el ends here
