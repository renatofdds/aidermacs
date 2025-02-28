;;; aidermacs-backend-vterm.el --- VTerm backend for aidermacs.el -*- lexical-binding: t; -*-
;; Author: Mingde (Matthew) Zeng <matthewzmd@posteo.net>
;; Version: 0.5.0
;; Keywords: ai emacs agents llm aider ai-pair-programming, convenience, tools
;; URL: https://github.com/MatthewZMD/aidermacs
;; Originally forked from: Kang Tu <tninja@gmail.com> Aider.el
;; SPDX-License-Identifier: Apache-2.0
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; VTerm backend implementation for aidermacs.el
;;
;;; Code:

(require 'vterm nil 'noerror)

;; we want to ensure these two variables are dynamic binding
(defvar vterm-shell)
(defvar vterm-buffer-name)

;; Forward declaration to avoid compiler warnings
(declare-function vterm--render "vterm")
(declare-function vterm--get-prompt-point "vterm")
(declare-function vterm-other-window "vterm")
(declare-function vterm-send-string "vterm")
(declare-function vterm-send-return "vterm")
(declare-function vterm-insert "vterm")


(defun aidermacs--is-aidermacs-vterm-buffer-p (&optional buffer)
  "Check if BUFFER is an aidermacs vterm buffer.
If BUFFER is nil, check the current buffer.
Returns non-nil if the buffer name matches the aidermacs buffer pattern."
  (let ((buf (or buffer (current-buffer))))
    (and (derived-mode-p 'vterm-mode)
         (string-match-p "^\\*aidermacs:" (buffer-name buf)))))

(defun aidermacs--vterm-check-finish-sequence-repeated (proc orig-filter start-point expected)
  "Check for the finish sequence in PROC's buffer.
PROC is the process to check.  ORIG-FILTER is the original process filter.
START-POINT is the starting position for output capture.  EXPECTED is the
pattern to match.  If the finish sequence is detected, store the output via
`aidermacs--store-output`, restore ORIG-FILTER, and return t."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      ;; Render vterm if the function is available
      (when (fboundp 'vterm--render)
        (condition-case nil
            (vterm--render)
          (error nil)))

      (let* ((prompt-point (condition-case nil
                               (vterm--get-prompt-point)
                             (error (point-max))))
             ;; Only do these expensive operations if we have a new prompt
             (has-new-prompt (< start-point prompt-point)))

        (when has-new-prompt
          ;; Only search for boundaries when we have a new prompt
          (let* ((seq-start (or (save-excursion
                                  (goto-char prompt-point)
                                  (condition-case nil
                                      (search-backward "\n" nil t)
                                    (error nil)))
                                (point-min)))
                 ;; Only get the prompt line, not the whole sequence
                 (prompt-line (buffer-substring-no-properties
                               seq-start
                               (min (+ seq-start 200) (point-max)))))

            (when (string-match-p expected prompt-line)
              (let ((output (buffer-substring-no-properties start-point seq-start)))
                (aidermacs--store-output (string-trim output)))
              (set-process-filter proc orig-filter)
              t)))))))

(defun aidermacs--vterm-output-advice (orig-fun &rest args)
  "Capture vterm output until the finish sequence appears.
ORIG-FUN is the original function being advised.  ARGS are its arguments.
This sets a temporary process filter that checks for the finish sequence
after each output chunk, reducing the need for timers."
  (if (aidermacs--is-aidermacs-vterm-buffer-p)
      (let* ((start-point (condition-case nil
                              (vterm--get-prompt-point)
                            (error (point-min))))
             (proc (get-buffer-process (current-buffer)))
             ;; Simplified pattern that just looks for a shell prompt
             (expected "^[^[:space:]]*>[[:space:]]")
             (orig-filter (process-filter proc)))
        ;; Set our temporary process filter.
        (set-process-filter
         proc
         (lambda (proc string)
           ;; Call the original filter.
           (funcall orig-filter proc string)

           ;; Check immediately after receiving output
           (when (aidermacs--vterm-check-finish-sequence-repeated proc orig-filter start-point expected)
             (when (timerp aidermacs--vterm-active-timer)
               (cancel-timer aidermacs--vterm-active-timer)
               (setq aidermacs--vterm-active-timer nil))
             (set-process-filter proc orig-filter))

           ;; If we haven't found it yet, set up a timer with adaptive frequency
           (unless aidermacs--vterm-active-timer
             (setq aidermacs--vterm-active-timer
                   (run-with-timer
                    0.05 0.05
                    (lambda ()
                      (when (aidermacs--vterm-check-finish-sequence-repeated
                             proc orig-filter start-point expected)
                        (when (timerp aidermacs--vterm-active-timer)
                          (cancel-timer aidermacs--vterm-active-timer)
                          (setq aidermacs--vterm-active-timer nil))
                        (set-process-filter proc orig-filter))))))))
        (apply orig-fun args))
    (apply orig-fun args)))

(defun aidermacs-run-vterm (program args buffer-name)
  "Create a vterm-based buffer and run aidermacs program.
PROGRAM is the command to run.  ARGS is a list of command line arguments.
BUFFER-NAME is the name for the vterm buffer."
  (unless (require 'vterm nil t)
    (error "Vterm package is not available"))
  (unless (get-buffer buffer-name)
    (let* ((mode (if (eq (frame-parameter nil 'background-mode) 'dark)
                     "--dark-mode"
                   "--light-mode"))
           (cmd (mapconcat #'identity (append (list program mode) args) " "))
           (vterm-buffer-name buffer-name)
           (vterm-shell cmd))
      (with-current-buffer (vterm-other-window)
        (advice-add 'vterm-send-return :around #'aidermacs--vterm-output-advice)
        ;; Set a reasonable scrollback limit to prevent memory issues
        (setq-local vterm-max-scrollback 5000)
        ;; Initialize timer variable
        (setq-local aidermacs--vterm-active-timer nil)
        ;; Set up multi-line key binding
        (let ((map (make-sparse-keymap)))
          (set-keymap-parent map (current-local-map))
          (define-key map (kbd aidermacs-vterm-multiline-newline-key) #'aidermacs-vterm-insert-newline)
          (use-local-map map))
        ;; Add cleanup hook
        (add-hook 'kill-buffer-hook #'aidermacs--vterm-cleanup nil t))))
  buffer-name)

(defvar-local aidermacs--vterm-active-timer nil
  "Store the active timer for vterm output processing.")

(defcustom aidermacs-vterm-multiline-newline-key "S-<return>"
  "Key binding to enter a newline without sending in vterm."
  :type 'string
  :group 'aidermacs)

(defun aidermacs--send-command-vterm (buffer command)
  "Send command to the aidermacs vterm buffer.
BUFFER is the target buffer to send to.  COMMAND is the text to send."
  (with-current-buffer buffer
    ;; Cancel any existing timer to prevent resource leaks
    (when aidermacs--vterm-active-timer
      (cancel-timer aidermacs--vterm-active-timer)
      (setq aidermacs--vterm-active-timer nil))
    (vterm-send-string command)
    (vterm-send-return)))

(defun aidermacs-vterm-insert-newline ()
  "Insert a newline in vterm without sending the command."
  (interactive)
  (vterm-insert "\n"))

(defun aidermacs--vterm-cleanup ()
  "Clean up vterm resources when buffer is killed."
  (when aidermacs--vterm-active-timer
    (cancel-timer aidermacs--vterm-active-timer)
    (setq aidermacs--vterm-active-timer nil)))

(provide 'aidermacs-backend-vterm)

;;; aidermacs-backend-vterm.el ends here
