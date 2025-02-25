;;; aidermacs-backend-vterm.el --- VTerm backend for aidermacs.el -*- lexical-binding: t; -*-
;; Author: Mingde (Matthew) Zeng <matthewzmd@posteo.net>
;; Version: 0.5.0
;; Package-Requires: ((emacs "28.1") (transient "0.8.4"))
;; Keywords: ai emacs agents llm aider ai-pair-programming, convenience, tools
;; URL: https://github.com/MatthewZMD/aidermacs.el
;; Originally forked from: Kang Tu <tninja@gmail.com> Aider.el

;;; Commentary:
;; VTerm backend implementation for aidermacs.el

;;; Code:

(require 'vterm nil 'noerror)

;; we want to ensure these two variables are dynamic binding
(defvar vterm-shell)
(defvar vterm-buffer-name)

(defun aidermacs--vterm-check-finish-sequence-repeated (proc orig-filter start-point expected)
  "Check for the finish sequence repeatedly in PROC's buffer.
PROC is the process to check.  ORIG-FILTER is the original process filter.
START-POINT is the starting position for output capture.  EXPECTED is the
pattern to match.  Forces a vterm render and redisplay.  If the finish
sequence is detected, store the output via `aidermacs--store-output`,
restore ORIG-FILTER, and return t."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      ;; Force vterm to update its display.
      (when (fboundp 'vterm--render)
        (vterm--render))
      (force-window-update (selected-window))
      (redisplay t)
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
          (let ((output (buffer-substring-no-properties start-point seq-start)))
            (aidermacs--store-output (string-trim output)))
          (set-process-filter proc orig-filter)
          t)))))

(defun aidermacs--vterm-output-advice (orig-fun &rest args)
  "Capture vterm output until the finish sequence appears.
ORIG-FUN is the original function being advised.  ARGS are its arguments.
This sets a temporary process filter and installs a repeating timer to
force vterm to update until the expected finish sequence is detected."
  (if (and (bound-and-true-p aidermacs-minor-mode)
           (eq major-mode 'vterm-mode))
      (let* ((start-point (vterm--get-prompt-point))
             (proc (get-buffer-process (current-buffer)))
             (expected "\n[^[:space:]]*>[[:space:]].*\n")
             (orig-filter (process-filter proc))
             (timer nil))
        ;; Set our temporary process filter.
        (set-process-filter
         proc
         (lambda (proc string)
           ;; Call the original filter.
           (funcall orig-filter proc string)
           ;; If we haven't yet started our repeating timer, do so.
           (unless timer
             (setq timer (run-with-timer
                          0.05 0.05
                          (lambda ()
                            (when (aidermacs--vterm-check-finish-sequence-repeated
                                   proc orig-filter start-point expected)
                              (cancel-timer timer)
                              (setq timer nil))))))))
        (apply orig-fun args))
    (apply orig-fun args)))

(defun aidermacs-run-vterm (program args buffer-name)
  "Create a vterm-based buffer and run aidermacs program.
PROGRAM is the command to run.  ARGS is a list of command line arguments.
BUFFER-NAME is the name for the vterm buffer."
  (unless (require 'vterm nil t)
    (error "vterm package is not available"))
  (unless (get-buffer buffer-name)
    (let* ((mode (if (eq (frame-parameter nil 'background-mode) 'dark)
                     "--dark-mode"
                   "--light-mode"))
           (cmd (mapconcat 'identity (append (list program mode) args) " "))
           (vterm-buffer-name buffer-name)
           (vterm-shell cmd)
           (vterm-shell-orig vterm-shell))
      (with-current-buffer (vterm-other-window)
        (aidermacs-minor-mode 1)
        (advice-add 'vterm-send-return :around #'aidermacs--vterm-output-advice))))
  buffer-name)

(defun aidermacs--send-command-vterm (buffer command)
  "Send command to the aidermacs vterm buffer.
BUFFER is the target buffer to send to.  COMMAND is the text to send."
  (with-current-buffer buffer
    (vterm-send-string command)
    (vterm-send-return)))

(provide 'aidermacs-backend-vterm)

;;; aidermacs-backend-vterm.el ends here
