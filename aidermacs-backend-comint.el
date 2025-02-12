;;; aidermacs-backend-comint.el --- Comint backend for aidermacs.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Comint backend implementation for aidermacs.el

;;; Code:

(require 'comint)

(defconst aidermacs-search-marker "<<<<<<< SEARCH")
(defconst aidermacs-diff-marker "=======")
(defconst aidermacs-replace-marker ">>>>>>> REPLACE")
(defconst aidermacs-fence-marker "```")
(defvar aidermacs-block-re
  (format "^\\(?:\\(?1:%s\\|%s\\)\\|\\(?1:%s\\).+\\)$" aidermacs-search-marker aidermacs-diff-marker aidermacs-fence-marker))

(defcustom aidermacs-comint-multiline-newline-key "S-<return>"
  "Key binding for `comint-accumulate' in Aidermacs buffers.
This allows for multi-line input without sending the command."
  :type 'string
  :group 'aidermacs)

(defface aidermacs-command-separator
  '((((type graphic)) :strike-through t :extend t)
    (((type tty)) :inherit font-lock-comment-face :underline t :extend t))
  "Face for command separator in aidermacs."
  :group 'aidermacs)

(defface aidermacs-command-text
  '((t :inherit bold))
  "Face for commands sent to aidermacs buffer."
  :group 'aidermacs)

(defface aidermacs-search-replace-block
  '((t :inherit 'diff-refine-added :bold t))
  "Face for search/replace block content."
  :group 'aidermacs)


(defvar aidermacs-font-lock-keywords
  '(("^\x2500+\n?" 0 '(face aidermacs-command-separator) t)
    ("^\x2500+" 0 '(face nil display (space :width 2)))
    ("^\\([0-9]+\\). " 0 font-lock-constant-face)
    ("^>>>>>>> REPLACE" 0 'aidermacs-search-replace-block t)
    ("^<<<<<<< SEARCH" 0 'aidermacs-search-replace-block t)
    ("^\\(```\\)\\([^[:space:]]*\\)" (1 'shadow t) (2 font-lock-builtin-face t))
    ("^=======$" 0 'aidermacs-search-replace-block t))
  "Font lock keywords for aidermacs buffer.")

;; Buffer-local variables for syntax highlighting state
(defvar-local aidermacs--syntax-block-delimiter nil
  "The expected delimiter (diff/replace/fence) for the current syntax block.")

(defvar-local aidermacs--syntax-block-start-pos nil
  "The starting position of the current syntax block.")

(defvar-local aidermacs--syntax-block-end-pos nil
  "The end position of the current syntax block.")

(defvar-local aidermacs--syntax-last-output-pos nil
  "Position tracker for incremental syntax highlighting.")

(defvar-local aidermacs--syntax-major-mode nil
  "The major mode used for syntax highlighting the current block.")

(defvar-local aidermacs--syntax-work-buffer nil
  "Temporary buffer used for syntax highlighting operations.")

(defun aidermacs-reset-font-lock-state ()
  "Reset font lock state to default for processing another a new src block."
  (unless (equal aidermacs--syntax-block-delimiter aidermacs-diff-marker)
    ;; if we are processing the other half of a SEARCH/REPLACE block, we need to
    ;; keep the mode
    (setq aidermacs--syntax-major-mode nil))
  (setq aidermacs--syntax-block-delimiter nil
        aidermacs--syntax-last-output-pos nil
        aidermacs--syntax-block-start-pos nil
        aidermacs--syntax-block-end-pos nil))

(defun aidermacs-fontify-blocks (_output)
  "Fontify search/replace blocks in comint output."
  (save-excursion
    (goto-char (or aidermacs--syntax-last-output-pos
                   comint-last-output-start))
    (beginning-of-line)

    ;; Continue processing existing block if we're in one
    (when aidermacs--syntax-block-start-pos
      (aidermacs--fontify-block))

    (setq aidermacs--syntax-last-output-pos nil)
    ;; Look for new blocks if we're not in one
    (while (and (null aidermacs--syntax-block-start-pos)
                (null aidermacs--syntax-last-output-pos)
                (re-search-forward aidermacs-block-re nil t))

      ;; If it is code fence marker, we need to check if there is a SEARCH marker
      ;; directly after it
      (when (equal (match-string 1) aidermacs-fence-marker)
        (let* ((next-line (min (point-max) (1+ (line-end-position))))
               (line-text (buffer-substring
                           next-line
                           (min (point-max) (+ next-line (length aidermacs-search-marker))))))
          (cond ((equal line-text aidermacs-search-marker)
                 ;; Next line is a SEARCH marker. use that instead of the fence marker
                 (re-search-forward (format "^\\(%s\\)" aidermacs-search-marker) nil t))
                ((string-prefix-p line-text aidermacs-search-marker)
                 ;; Next line *might* be a SEARCH marker. Don't process more of
                 ;; the buffer until we know for sure
                 (setq aidermacs--syntax-last-output-pos comint-last-output-start)))))

      (unless aidermacs--syntax-last-output-pos
        ;; Set up new block state
        (setq aidermacs--syntax-block-delimiter
              (pcase (match-string 1)
                ((pred (equal aidermacs-search-marker)) aidermacs-diff-marker)
                ((pred (equal aidermacs-diff-marker)) aidermacs-replace-marker)
                ((pred (equal aidermacs-fence-marker)) aidermacs-fence-marker))
              aidermacs--syntax-block-start-pos (line-end-position)
              aidermacs--syntax-block-end-pos (line-end-position)
              aidermacs--syntax-major-mode (aidermacs--guess-major-mode))

        ;; Set the major-mode of the font lock buffer
        (let ((mode aidermacs--syntax-major-mode))
          (with-current-buffer aidermacs--syntax-work-buffer
            (erase-buffer)
            (unless (eq mode major-mode)
              (condition-case e
                  (let ((inhibit-message t))
                    (funcall mode))
                (error "aidermacs: failed to init major-mode `%s' for font-locking: %s" mode e)))))

        ;; Process initial content
        (aidermacs--fontify-block)))))

(defun aidermacs--fontify-block ()
  "Fontify as much of the current source block as possible."
  (let* ((last-bol (save-excursion
                     (goto-char (point-max))
                     (line-beginning-position)))
         (last-output-start aidermacs--block-end)
         end-of-block-p)

    (setq aidermacs--block-end
          (cond ((re-search-forward (concat "^" aidermacs--block-end-marker "$") nil t)
                 ;; Found the end of the block
                 (setq end-of-block-p t)
                 (line-beginning-position))
                ((string-prefix-p (buffer-substring last-bol (point-max)) aidermacs--block-end-marker)
                 ;; The end of the text *might* be the end marker. back up to
                 ;; make sure we don't process it until we know for sure
                 last-bol)
                ;; We can process till the end of the text
                (t (point-max))))

    ;; Append new content to temp buffer and fontify
    (let ((new-content (buffer-substring-no-properties
                        last-output-start
                        aidermacs--block-end))
          (pos aidermacs--block-start)
          (font-pos 0)
          fontified)

      ;; Insert the new text and get the fontified result
      (with-current-buffer aidermacs--syntax-work-buffer
        (goto-char (point-max))
        (insert new-content)
        (with-demoted-errors "aidermacs block font lock error: %s"
          (let ((inhibit-message t))
            (font-lock-ensure)))
        (setq fontified (buffer-string)))

      ;; Apply the faces to the buffer
      (remove-overlays aidermacs--syntax-block-start-pos aidermacs--syntax-block-end-pos)
      (while (< pos aidermacs--syntax-block-end-pos)
        (let* ((next-font-pos (or (next-property-change font-pos fontified) (length fontified)))
               (next-pos (+ aidermacs--syntax-block-start-pos next-font-pos))
               (face (get-text-property font-pos 'face fontified)))
          (ansi-color-apply-overlay-face pos next-pos face)
          (setq pos next-pos
                font-pos next-font-pos))))

    ;; If we found the end marker, finalize the block
    (when end-of-block-p
      (when (equal aidermacs--syntax-block-delimiter aidermacs-diff-marker)
        ;; we will need to process the other half of the SEARCH/REPLACE block.
        ;; Backup so it will get matched
        (beginning-of-line))
      (aidermacs-reset-font-lock-state))))

(defun aidermacs--guess-major-mode ()
  "Extract the major mode from fence markers or filename."
  (save-excursion
    (beginning-of-line)
    (or
     ;; check if the block has a language id
     (when (let ((re "^```\\([^[:space:]]+\\)"))
             (or (looking-at re)
                 (save-excursion
                   (forward-line -1)
                   ;; check the previous line since this might be a SEARCH block
                   (looking-at re))))
       (let* ((lang (downcase (match-string 1)))
              (mode (map-elt aidermacs-language-name-map lang lang)))
         (intern-soft (concat mode "-mode"))))
     ;; check the file extension in auto-mode-alist
     (when (re-search-backward "^\\([^[:space:]]+\\)" (line-beginning-position -3) t)
       (let ((file (match-string 1)))
         (cdr (cl-assoc-if (lambda (re) (string-match re file)) auto-mode-alist))))
     aidermacs--syntax-major-mode
     'fundamental-mode)))

(defun aidermacs-kill-buffer ()
  "Clean-up fontify buffer."
  (when (bufferp aidermacs--syntax-work-buffer)
    (kill-buffer aidermacs--syntax-work-buffer)))

(defvar-local aidermacs--comint-output-temp ""
  "Temporary output variable storing the output log.")

(defun aidermacs-input-sender (proc string)
  "Reset font-lock state before executing a command."
  (aidermacs-reset-font-lock-state)
  (comint-simple-send proc (aidermacs--process-message-if-multi-line string)))

(defun aidermacs--comint-output-filter (output)
  "Accumulate OUTPUT until a prompt is detected, then store it."
  (unless (string-empty-p output)
    (setq aidermacs--comint-output-temp
          (concat aidermacs--comint-output-temp output))
    ;; Check if the output contains a prompt
    (when (string-match-p "\n[^[:space:]]*>[[:space:]]$" aidermacs--comint-output-temp)
      (aidermacs--store-output aidermacs--comint-output-temp)
      (setq aidermacs--comint-output-temp ""))))

(defun aidermacs-run-aidermacs-comint (program args buffer-name)
  "Create a comint-based buffer and run aidermacs PROGRAM with ARGS in BUFFER-NAME."
  (let ((comint-terminfo-terminal "dumb"))
    (unless (comint-check-proc buffer-name)
      (apply 'make-comint-in-buffer "aidermacs" buffer-name program nil args)
      (with-current-buffer buffer-name
        (comint-mode)
        (setq-local comint-input-sender 'aidermacs-input-sender)
        (setq aidermacs--syntax-work-buffer
              (get-buffer-create (concat " *aidermacs-syntax" buffer-name)))
        (add-hook 'kill-buffer-hook #'aidermacs-kill-buffer nil t)
        (add-hook 'comint-output-filter-functions #'aidermacs-fontify-blocks 100 t)
        (add-hook 'comint-output-filter-functions #'aidermacs--comint-output-filter)
        (let ((local-map (make-sparse-keymap)))
          (set-keymap-parent local-map comint-mode-map)
          (define-key local-map (kbd aidermacs-comint-multiline-newline-key) #'comint-accumulate)
          (use-local-map local-map))
        (font-lock-add-keywords nil aidermacs-font-lock-keywords t)))))

(defun aidermacs--send-command-comint (buffer command &optional switch-to-buffer)
  "Send COMMAND to the aidermacs comint BUFFER.
If SWITCH-TO-BUFFER is non-nil, switch to the buffer after sending."
  (with-current-buffer buffer
    (let ((process (get-buffer-process buffer))
          (inhibit-read-only t))
      ;; Store command before sending
      (setq aidermacs--last-command command
            aidermacs--current-output nil)
      (goto-char (process-mark process))
      (aidermacs-reset-font-lock-state)
      (insert (propertize command
                          'face 'aidermacs-command-text
                          'font-lock-face 'aidermacs-command-text
                          'rear-nonsticky t))
      (set-marker (process-mark process) (point))
      (comint-send-string process (concat command "\n")))))

(provide 'aidermacs-backend-comint)

;;; aidermacs-backend-comint.el ends here
