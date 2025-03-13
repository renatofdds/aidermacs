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

(defun aidermacs--verify-tracked-files ()
  "Verify files in `aidermacs--tracked-files` exist.
Remove any files that don't exist."
  (let* ((project-root (aidermacs-project-root))
         (is-remote (file-remote-p project-root))
         (valid-files nil))
    (dolist (file aidermacs--tracked-files)
      (let* ((is-readonly (string-match-p " (read-only)$" file))
             (actual-file (if is-readonly
                              (substring file 0 (- (length file) 12))
                            file))
             (full-path (expand-file-name actual-file project-root)))
        (when (or (file-exists-p full-path) is-remote)
          (push file valid-files))))
    (setq aidermacs--tracked-files valid-files)))

(defun aidermacs--parse-output-for-files (output)
  "Parse OUTPUT for files and add them to `aidermacs--tracked-files'."
  (when output
    (let ((lines (split-string output "\n"))
          (last-line "")
          (in-udiff nil)
          (current-udiff-file nil))
      (dolist (line lines)
        (cond
         ;; Applied edit to <filename>
         ((string-match "Applied edit to \\(\\./\\)?\\(.+\\)" line)
          (when-let ((file (match-string 2 line)))
            (add-to-list 'aidermacs--tracked-files file)))

         ;; Added <filename> to the chat.
         ((string-match "Added \\(\\./\\)?\\(.+\\) to the chat" line)
          (when-let ((file (match-string 2 line)))
            (add-to-list 'aidermacs--tracked-files file)))

         ;; Removed <filename> from the chat (with or without ./ prefix)
         ((string-match "Removed \\(\\./\\)?\\(.+\\) from the chat" line)
          (when-let ((file (match-string 2 line)))
            (setq aidermacs--tracked-files (delete file aidermacs--tracked-files))))

         ;; Added <filename> to read-only files.
         ((string-match "Added \\(\\./\\)?\\(.+\\) to read-only files" line)
          (when-let ((file (match-string 2 line)))
            (add-to-list 'aidermacs--tracked-files (concat file " (read-only)"))))

         ;; Moved <file> from editable to read-only files in the chat
         ((string-match "Moved \\(\\./\\)?\\(.+\\) from editable to read-only files in the chat" line)
          (when-let ((file (match-string 2 line)))
            (let ((editable-file (replace-regexp-in-string " (read-only)$" "" file)))
              (setq aidermacs--tracked-files (delete editable-file aidermacs--tracked-files))
              (add-to-list 'aidermacs--tracked-files (concat file " (read-only)")))))

         ;; Moved <file> from read-only to editable files in the chat
         ((string-match "Moved \\(\\./\\)?\\(.+\\) from read-only to editable files in the chat" line)
          (when-let ((file (match-string 2 line)))
            (let ((read-only-file (concat file " (read-only)")))
              (setq aidermacs--tracked-files (delete read-only-file aidermacs--tracked-files))
              (add-to-list 'aidermacs--tracked-files file))))

         ;; <file>\nAdd file to the chat?
         ((string-match "Add file to the chat?" line)
          (add-to-list 'aidermacs--tracked-files last-line)
          (aidermacs--prepare-for-code-edit))

         ;; <file> is already in the chat as an editable file
         ((string-match "\\(\\./\\)?\\(.+\\) is already in the chat as an editable file" line)
          (when-let ((file (match-string 2 line)))
            (add-to-list 'aidermacs--tracked-files file)))

         ;; Handle udiff format
         ;; Detect start of udiff with "--- filename"
         ((string-match "^--- \\(\\./\\)?\\(.+\\)" line)
          (setq in-udiff t
                current-udiff-file (match-string 2 line)))

         ;; Confirm udiff file with "+++ filename" line
         ((and in-udiff
               current-udiff-file
               (string-match "^\\+\\+\\+ \\(\\./\\)?\\(.+\\)" line))
          (let ((plus-file (match-string 2 line)))
            ;; Only add if the filenames match (ignoring ./ prefix)
            (when (string= (file-name-nondirectory current-udiff-file)
                           (file-name-nondirectory plus-file))
              (add-to-list 'aidermacs--tracked-files current-udiff-file)
              (setq in-udiff nil
                    current-udiff-file nil)))))

        (setq last-line line))

      ;; Verify all tracked files exist
      (aidermacs--verify-tracked-files))))

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
