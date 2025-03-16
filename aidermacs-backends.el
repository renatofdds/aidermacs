;;; aidermacs-backends.el --- Backend dispatcher for aidermacs -*- lexical-binding: t; -*-
;; Author: Mingde (Matthew) Zeng <matthewzmd@posteo.net>
;; Version: 1.0.0
;; Keywords: ai emacs llm aider ai-pair-programming tools
;; URL: https://github.com/MatthewZMD/aidermacs
;; SPDX-License-Identifier: Apache-2.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Backend dispatcher for Aidermacs, allowing choice between
;; different backend implementations for Aider process.
;; Supports Comint and VTerm backends.
;;
;; Features:
;; - Backend selection via `aidermacs-backend` variable
;; - Abstracts backend-specific Aider functions
;; - Manages output history and callbacks

;; Originally forked from: Kang Tu <tninja@gmail.com> Aider.el

;;; Code:

(require 'aidermacs-backend-comint)
(when (commandp 'vterm)
  (require 'aidermacs-backend-vterm))

(declare-function aidermacs-run-vterm "aidermacs-backend-vterm" (program args buffer-name))
(declare-function aidermacs--send-command-vterm "aidermacs-backend-vterm" (buffer command))
(declare-function aidermacs-project-root "aidermacs" ())
(declare-function aidermacs--prepare-for-code-edit "aidermacs" ())
(declare-function aidermacs--get-files-in-session "aidermacs" (callback))

(defcustom aidermacs-backend 'comint
  "Backend to use for the aidermacs process.
Options are `comint' (the default) or `vterm'.  When set to `vterm',
aidermacs launches a fully functional vterm buffer instead
of using a comint process."
  :type '(choice (const :tag "Comint" comint)
                 (const :tag "VTerm" vterm)))

(defcustom aidermacs-output-limit 10
  "Maximum number of output entries to keep in history."
  :type 'integer)

(defvar-local aidermacs--output-history nil
  "List to store aidermacs output history.
Each entry is a cons cell (timestamp . output-text).")

(defvar-local aidermacs--last-command nil
  "Store the last command sent to aidermacs.")

(defvar-local aidermacs--current-output ""
  "Accumulator for current output being captured as a string.")

(defcustom aidermacs-before-run-backend-hook nil
  "Hook run before the aidermacs backend is startd."
  :type 'hook)

(defun aidermacs-get-output-history (&optional limit)
  "Get the output history, optionally limited to LIMIT entries.
LIMIT is the maximum number of entries to return.
Returns a list of (timestamp . output-text) pairs, most recent first."
  (let ((history aidermacs--output-history))
    (if limit
        (seq-take history limit)
      history)))

(defun aidermacs-clear-output-history ()
  "Clear the output history."
  (interactive)
  (setq aidermacs--output-history nil))

(defvar-local aidermacs--current-callback nil
  "Store the callback function for the current command.")

(defvar-local aidermacs--in-callback nil
  "Flag to prevent recursive callbacks.")

(defvar-local aidermacs--tracked-files nil
  "List of files that have been mentioned in the aidermacs output.
This is used to avoid having to run /ls repeatedly.")

(defun aidermacs--parse-output-for-files (output)
  "Parse OUTPUT for files and add them to `aidermacs--tracked-files'."
  (when output
    (let ((tracked-files aidermacs--tracked-files))
      (with-temp-buffer
        (insert output)
        (goto-char (point-min))

        ;; Applied edit to <filename>
        (while (search-forward "Applied edit to" nil t)
          (beginning-of-line)
          (when-let ((file (and (looking-at ".*Applied edit to \\(\\./\\)?\\(.+\\)")
                                (match-string-no-properties 2))))
            (add-to-list 'tracked-files file))
          (forward-line 1))

        ;; Combined file tracking logic
        (goto-char (point-min))
        (while (re-search-forward
                "\\(Added\\|Removed\\|Moved\\) \\(\\./\\)?\\([^ ]+\\) \\(to\\|from\\) \\(the chat\\|editable\\|read-only\\) files?"
                nil t)
          (let* ((action (match-string 1))
                 (file (match-string 3))
                 (state (match-string 5)))
            (cond
             ;; Added files
             ((string= action "Added")
              (add-to-list 'tracked-files
                           (if (string= state "read-only")
                               (concat file " (read-only)")
                             file)))

             ;; Removed files
             ((string= action "Removed")
              (setq tracked-files (delete file tracked-files)))

             ;; Moved files
             ((string= action "Moved")
              (let* ((from-state (if (string= state "editable") "read-only" "editable"))
                     (old-file (if (string= from-state "read-only")
                                   (concat file " (read-only)")
                                 file))
                     (new-file (if (string= state "read-only")
                                   (concat file " (read-only)")
                                 file)))
                (setq tracked-files (delete old-file tracked-files))
                (add-to-list 'tracked-files new-file))))))

        ;; <file> is already in the chat as an editable file
        (goto-char (point-min))
        (while (search-forward " is already in the chat as an editable file" nil t)
          (beginning-of-line)
          (when-let ((file (and (looking-at "\\(\\./\\)?\\(.+\\) is already in the chat as an editable file")
                                (match-string-no-properties 2))))
            (add-to-list 'tracked-files file))
          (forward-line 1))

        ;; Add file to the chat?
        (goto-char (point-min))
        (while (search-forward "Add file to the chat?" nil t)
          (save-excursion
            (forward-line -1)
            (let ((potential-file (string-trim (buffer-substring (line-beginning-position) (line-end-position)))))
              (when (not (string-empty-p potential-file))
                (add-to-list 'tracked-files potential-file)
                (aidermacs--prepare-for-code-edit))))
          (forward-line 1))

        ;; Handle udiff format
        (goto-char (point-min))
        (while (search-forward "--- " nil t)
          (message "processing %s " tracked-files)
          (let* ((line-end (line-end-position))
                 (current-udiff-file (when (looking-at "\\(\\./\\)?\\(.+\\)")
                                       (match-string-no-properties 2))))
            (when current-udiff-file
              (forward-line 1)
              (when (looking-at "\\+\\+\\+ \\(\\./\\)?\\(.+\\)")
                (let ((plus-file (match-string-no-properties 2)))
                  (when (string= (file-name-nondirectory current-udiff-file)
                                 (file-name-nondirectory plus-file))
                    (add-to-list 'tracked-files current-udiff-file))))))))

      ;; Verify all tracked files exist
      (let* ((project-root (aidermacs-project-root))
             (is-remote (file-remote-p project-root))
             (valid-files nil))
        (dolist (file tracked-files)
          (let* ((is-readonly (string-match-p " (read-only)$" file))
                 (actual-file (if is-readonly
                                  (substring file 0 (- (length file) 12))
                                file))
                 (full-path (expand-file-name actual-file project-root)))
            (when (or (file-exists-p full-path) is-remote)
              (push file valid-files))))
        (setq tracked-files valid-files))
      (setq aidermacs--tracked-files tracked-files))))

(defun aidermacs-reset-tracked-files ()
  "Reset the list of tracked files and force a refresh."
  (interactive)
  (setq aidermacs--tracked-files nil)
  (aidermacs--get-files-in-session (lambda (files)
                                     (message "Refreshed file list: %s" files))))

(defun aidermacs--store-output (output)
  "Store output string in the history with timestamp.
OUTPUT is the string to store.
If there's a callback function, call it with the output."
  (when (stringp output)
    ;; Store the output
    (setq aidermacs--current-output (substring-no-properties output))
    (push (cons (current-time) (substring-no-properties output)) aidermacs--output-history)
    ;; Trim history if needed
    (when (> (length aidermacs--output-history) aidermacs-output-limit)
      (setq aidermacs--output-history
            (seq-take aidermacs--output-history aidermacs-output-limit)))
    ;; Parse files from output
    (aidermacs--parse-output-for-files output)
    ;; Handle callback if present
    (unless aidermacs--in-callback
      (when (functionp aidermacs--current-callback)
        (let ((aidermacs--in-callback t))
          (funcall aidermacs--current-callback)
          (setq aidermacs--current-callback nil))))))

;; Backend dispatcher functions
(defun aidermacs-run-backend (program args buffer-name)
  "Run aidermacs using the selected backend.
PROGRAM is the aidermacs executable path.  ARGS are command line arguments.
BUFFER-NAME is the name for the aidermacs buffer."
  (message "Running %s with %s" program args)
  (run-hooks 'aidermacs-before-run-backend-hook)
  (cond
   ((eq aidermacs-backend 'vterm)
    (aidermacs-run-vterm program args buffer-name))
   (t
    (aidermacs-run-comint program args buffer-name))))

(defun aidermacs--is-aidermacs-buffer-p (&optional buffer)
  "Check if BUFFER is any type of aidermacs buffer.
If BUFFER is nil, check the current buffer.
Returns non-nil if the buffer has either `aidermacs-comint-mode' or
`aidermacs-vterm-mode' enabled."
  (let ((buf (or buffer (current-buffer))))
    (with-current-buffer buf
      (or (bound-and-true-p aidermacs-comint-mode)
          (bound-and-true-p aidermacs-vterm-mode)))))

(defun aidermacs--send-command-backend (buffer command &optional redirect)
  "Send command to buffer using the appropriate backend.
BUFFER is the target buffer.  COMMAND is the text to send.
If REDIRECT is non-nil it redirects the output (hidden) for comint backend."
  (if (eq aidermacs-backend 'vterm)
      (aidermacs--send-command-vterm buffer command)
    (if redirect
        (aidermacs--send-command-redirect-comint buffer command)
      (aidermacs--send-command-comint buffer command))))

(provide 'aidermacs-backends)

;;; aidermacs-backends.el ends here
