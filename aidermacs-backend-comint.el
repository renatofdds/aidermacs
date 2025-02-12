;;; aidermacs-backend-comint.el --- Comint backend for aidermacs.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Comint backend implementation for aidermacs.el

;;; Code:

(require 'comint)

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

(defvar-local aidermacs--font-lock-buffer nil
  "Temporary buffer for fontification.")

(defun aidermacs--comint-output-filter (output)
  "Filter function for comint OUTPUT."
  (when (not (string-empty-p output))
    (aidermacs--store-output output)))

(defun aidermacs-reset-font-lock-state ()
  "Reset font lock state to default for processing another a new src block."
  (unless (equal aidermacs--block-end-marker aidermacs-diff-marker)
    ;; if we are processing the other half of a SEARCH/REPLACE block, we need to
    ;; keep the mode
    (setq aidermacs--block-mode nil))
  (setq aidermacs--block-end-marker nil
        aidermacs--last-output-start nil
        aidermacs--block-start nil
        aidermacs--block-end nil))

(defun aidermacs-fontify-blocks (_output)
  "Fontify search/replace blocks in comint output."
  (save-excursion
    (goto-char (or aidermacs--last-output-start
                   comint-last-output-start))
    (beginning-of-line)

    ;; Continue processing existing block if we're in one
    (when aidermacs--block-start
      (aidermacs--fontify-block))

    (setq aidermacs--last-output-start nil)
    ;; Look for new blocks if we're not in one
    (while (and (null aidermacs--block-start)
                (null aidermacs--last-output-start)
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
                 (setq aidermacs--last-output-start comint-last-output-start)))))

      (unless aidermacs--last-output-start
        ;; Set up new block state
        (setq aidermacs--block-end-marker
              (pcase (match-string 1)
                ((pred (equal aidermacs-search-marker)) aidermacs-diff-marker)
                ((pred (equal aidermacs-diff-marker)) aidermacs-replace-marker)
                ((pred (equal aidermacs-fence-marker)) aidermacs-fence-marker))
              aidermacs--block-start (line-end-position)
              aidermacs--block-end (line-end-position)
              aidermacs--block-mode (aidermacs--guess-major-mode))

        ;; Set the major-mode of the font lock buffer
        (let ((mode aidermacs--block-mode))
          (with-current-buffer aidermacs--font-lock-buffer
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
      (with-current-buffer aidermacs--font-lock-buffer
        (goto-char (point-max))
        (insert new-content)
        (with-demoted-errors "aidermacs block font lock error: %s"
          (let ((inhibit-message t))
            (font-lock-ensure)))
        (setq fontified (buffer-string)))

      ;; Apply the faces to the buffer
      (remove-overlays aidermacs--block-start aidermacs--block-end)
      (while (< pos aidermacs--block-end)
        (let* ((next-font-pos (or (next-property-change font-pos fontified) (length fontified)))
               (next-pos (+ aidermacs--block-start next-font-pos))
               (face (get-text-property font-pos 'face fontified)))
          (ansi-color-apply-overlay-face pos next-pos face)
          (setq pos next-pos
                font-pos next-font-pos))))

    ;; If we found the end marker, finalize the block
    (when end-of-block-p
      (when (equal aidermacs--block-end-marker aidermacs-diff-marker)
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
     aidermacs--block-mode
     'fundamental-mode)))

(defun aidermacs-run-aidermacs-comint (program args buffer-name)
  "Create a comint-based buffer and run aidermacs PROGRAM with ARGS in BUFFER-NAME."
  (let ((comint-terminfo-terminal "dumb"))
    (unless (comint-check-proc buffer-name)
      (apply 'make-comint-in-buffer "aidermacs" buffer-name program nil args)
      (with-current-buffer buffer-name
        (comint-mode)
        (setq-local comint-input-sender 'aidermacs-input-sender)
        (setq aidermacs--font-lock-buffer
              (get-buffer-create (concat " *aidermacs-fontify" buffer-name)))
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
      (insert (propertize command
                          'face 'aidermacs-command-text
                          'font-lock-face 'aidermacs-command-text
                          'rear-nonsticky t))
      (set-marker (process-mark process) (point))
      (comint-send-string process (concat command "\n")))))

(provide 'aidermacs-backend-comint)

;;; aidermacs-backend-comint.el ends here
