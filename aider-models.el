;;; aider-models.el --- Model selection for aider.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Model selection functionality for aider.el

;;; Code:

(defgroup aider-models nil
  "Model selection customization for aider."
  :group 'aider)

(defcustom aider-popular-models
  '("anthropic/claude-3-5-sonnet-20241022"  ;; really good in practical
    "o3-mini" ;; very powerful
    "gemini/gemini-exp-1206"  ;; free
    "r1"  ;; performance match o1, price << claude sonnet. weakness: small context
    "deepseek/deepseek-chat"  ;; chatgpt-4o level performance, price is 1/100. weakness: small context
    )
  "List of available AI models for selection.
Each model should be in the format expected by the aider command line interface.
Also based on aider LLM benchmark: https://aider.chat/docs/leaderboards/"
  :type '(repeat string)
  :group 'aider-models)

(defun aider--select-model ()
  "Private function for model selection with completion."
  (completing-read "Select AI model: " aider-popular-models nil t nil nil (car aider-popular-models)))

;;;###autoload
(defun aider-change-model ()
  "Interactively select and change AI model in current aider session."
  (interactive)
  (let ((model (aider--select-model)))
    (when model
      (aider--send-command (format "/model %s" model) t))))

(provide 'aider-models)

;;; aider-models.el ends here
