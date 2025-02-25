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
  "Check for the finish sequence in PROC's buffer.
PROC is the process to check.  ORIG-FILTER is the original process filter.
START-POINT is the starting position for output capture.  EXPECTED is the
pattern to match.  If the finish sequence is detected, store the output via
`aidermacs--store-output`, restore ORIG-FILTER, and return t."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      ;; Only render vterm if needed - this is expensive
      (when (and (fboundp 'vterm--render)
                 (vterm--invalidate-p))
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
  (if (and (bound-and-true-p aidermacs-minor-mode)
           (eq major-mode 'vterm-mode))
      (let* ((start-point (condition-case nil
                              (vterm--get-prompt-point)
                            (error (point-min))))
             (proc (get-buffer-process (current-buffer)))
             ;; Simplified pattern that just looks for a shell prompt
             (expected "^[^[:space:]]*>[[:space:]]")
             (orig-filter (process-filter proc))
             (timer nil))
        ;; Set our temporary process filter.
        (set-process-filter
         proc
         (lambda (proc string)
           ;; Call the original filter.
           (funcall orig-filter proc string)
           
           ;; Check immediately after receiving output
           (when (aidermacs--vterm-check-finish-sequence-repeated
                  proc orig-filter start-point expected)
             (when timer
               (cancel-timer timer)
               (setq timer nil))
             (return))
           
           ;; If we haven't found it yet, set up a timer with adaptive frequency
           (unless timer
             (setq timer (run-with-timer
                          0.05 0.05
                          (lambda ()
                            (cond
                             ;; Found the prompt, we're done
                             ((aidermacs--vterm-check-finish-sequence-repeated
                               proc orig-filter start-point expected)
                              (cancel-timer timer)
                              (setq timer nil))
                             
                             ;; Just keep checking until we find the prompt
                             )))))))
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
        (advice-add 'vterm-send-return :around #'aidermacs--vterm-output-advice)
        ;; Set a reasonable scrollback limit to prevent memory issues
        (setq-local vterm-max-scrollback 5000)
        ;; Set up multi-line key binding
        (let ((map (make-sparse-keymap)))
          (set-keymap-parent map (current-local-map))
          (define-key map (kbd aidermacs-vterm-multiline-newline-key) #'aidermacs-vterm-insert-newline)
          (define-key map (kbd aidermacs-vterm-multiline-send-key) #'aidermacs-vterm-send-multi-line)
          (define-key map (kbd "C-c C-k") #'aidermacs-vterm-cancel-multi-line)
          (use-local-map map))
        ;; Add cleanup hook
        (add-hook 'kill-buffer-hook #'aidermacs--vterm-cleanup nil t))))
  buffer-name)

(defvar-local aidermacs--vterm-active-timer nil
  "Store the active timer for vterm output processing.")

(defvar-local aidermacs--vterm-multi-line-input nil
  "Accumulated multi-line input in vterm mode.")

(defvar-local aidermacs--vterm-multi-line-mode nil
  "Non-nil when in multi-line input mode in vterm.")

(defcustom aidermacs-vterm-multiline-newline-key "S-<return>"
  "Key binding to enter a newline without sending in vterm."
  :type 'string
  :group 'aidermacs)

(defcustom aidermacs-vterm-multiline-send-key "C-<return>"
  "Key binding to send multi-line input in vterm mode."
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
    ;; Reset multiline mode if active
    (aidermacs-vterm-reset-multi-line-state)
    (vterm-send-string command)
    (vterm-send-return)))

(defun aidermacs-vterm-insert-newline ()
  "Insert a newline in vterm multi-line input."
  (interactive)
  (if aidermacs--vterm-multi-line-mode
      (progn
        (setq aidermacs--vterm-multi-line-input 
              (concat aidermacs--vterm-multi-line-input "\n"))
        (let ((inhibit-read-only t))
          (vterm-insert "\n")))
    ;; If not in multi-line mode, enter it
    (setq aidermacs--vterm-multi-line-mode t
          aidermacs--vterm-multi-line-input "")
    (let ((inhibit-read-only t))
      (vterm-insert "\n[multi-line mode] (Use Shift+Enter for new line, Ctrl+Enter to send)\n"))))

(defun aidermacs-vterm-send-multi-line ()
  "Send accumulated multi-line input in vterm."
  (interactive)
  (when aidermacs--vterm-multi-line-mode
    (let ((input (string-trim aidermacs--vterm-multi-line-input)))
      (setq aidermacs--vterm-multi-line-mode nil
            aidermacs--vterm-multi-line-input nil)
      ;; Format and send the input
      (vterm-send-string (format "{aidermacs\n%s\naidermacs}" input))
      (vterm-send-return))))

(defun aidermacs-vterm-cancel-multi-line ()
  "Cancel multiline input mode in vterm."
  (interactive)
  (when aidermacs--vterm-multi-line-mode
    (setq aidermacs--vterm-multi-line-mode nil
          aidermacs--vterm-multi-line-input nil)
    (let ((inhibit-read-only t))
      (vterm-insert "\n[multi-line mode canceled]\n"))))

(defun aidermacs-vterm-reset-multi-line-state ()
  "Reset multi-line state variables."
  (setq aidermacs--vterm-multi-line-mode nil
        aidermacs--vterm-multi-line-input nil))

(defun aidermacs--vterm-cleanup ()
  "Clean up vterm resources when buffer is killed."
  (when aidermacs--vterm-active-timer
    (cancel-timer aidermacs--vterm-active-timer)
    (setq aidermacs--vterm-active-timer nil))
  (aidermacs-vterm-reset-multi-line-state))

(provide 'aidermacs-backend-vterm)

;;; aidermacs-backend-vterm.el ends here
