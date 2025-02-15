;;; aidermacs-helm.el --- Helm completion for aidermacs.el -*- lexical-binding: t; -*-
;; Author: Mingde (Matthew) Zeng <matthewzmd@posteo.net>
;; Version: 0.5.0
;; Package-Requires: ((emacs "26.1") (helm "3.9.0"))
;; Keywords: ai emacs agents llm aider ai-pair-programming, convenience, tools
;; URL: https://github.com/MatthewZMD/aidermacs.el
;; Originally forked from: Kang Tu <tninja@gmail.com> Aider.el

;;; Commentary:
;; Optional Helm completion interface for aidermacs.el
;; To use this, ensure both aidermacs.el and helm are installed.

;;; Code:

(require 'helm)
(require 'cl-lib)  ; For `cl-subseq`

(defun aidermacs-helm-read-string (prompt &optional initial-input)
  "Read a string with Helm completion for aidermacs, showing historical inputs.
PROMPT is the prompt string.
INITIAL-INPUT is optional initial input string."
  (let ((input (helm-comp-read
                prompt
                aidermacs-read-string-history
                :must-match nil
                :name "Aidermacs Input"
                :fuzzy t
                :initial-input initial-input)))
    ;; Add to history if non-empty
    (unless (string-empty-p input)
      (add-to-history 'aidermacs-read-string-history input))
    input))

(declare-function aidermacs-read-string "aidermacs")

;;;###autoload
(with-eval-after-load 'aidermacs
  (if (featurep 'helm)
      (defalias 'aidermacs-read-string 'aidermacs-helm-read-string)
    (message "Helm is not available. Please install helm package to use aidermacs-helm features")))

(provide 'aidermacs-helm)
;;; aidermacs-helm.el ends here
