;;; aidermacs-models.el --- Model selection for aidermacs -*- lexical-binding: t; -*-
;; Author: Mingde (Matthew) Zeng <matthewzmd@posteo.net>
;; Version: 1.0
;; Keywords: ai emacs llm aider ai-pair-programming tools
;; URL: https://github.com/MatthewZMD/aidermacs
;; SPDX-License-Identifier: Apache-2.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides model selection for Aidermacs, allowing choice between
;; different AI models for Aider sessions. Supports fetching models
;; from various API providers and caching for faster access.
;;
;; Features:
;; - Model selection via completing-read interface
;; - Fetch models from OpenAI-compatible APIs
;; - Model caching for faster access
;; - Custom default models for different tasks

;; Originally forked from: Kang Tu <tninja@gmail.com> Aider.el

;;; Code:

(require 'json)
(require 'url)

(defvar url-http-end-of-headers)

(declare-function aidermacs--send-command "aidermacs")
(declare-function aidermacs-buffer-name "aidermacs")
(declare-function aidermacs-exit "aidermacs")

(defvar aidermacs--current-output)
(defvar aidermacs-use-architect-mode)

(defgroup aidermacs-models nil
  "Model selection for Aidermacs."
  :group 'aidermacs)

(defcustom aidermacs-default-model "sonnet"
  "Default AI model to use for aidermacs sessions when not in Architect mode."
  :type 'string)

(defcustom aidermacs-architect-model aidermacs-default-model
  "Default reasoning AI model to use for architect mode.
Defaults to `aidermacs-default-model' if not explicitly set."
  :type 'string)

(defcustom aidermacs-editor-model aidermacs-default-model
  "Default editing AI model to use for architect mode.
Defaults to `aidermacs-default-model' if not explicitly set."
  :type 'string)

(defcustom aidermacs-popular-models
  '("sonnet"
    "o1-mini"
    "gemini/gemini-2.0-flash"
    "r1"
    "deepseek/deepseek-chat")
  "List of available AI models for selection.
Each model should be in the format expected by the aidermacs CLI.
Also based on aidermacs LLM benchmark: https://aidermacs.chat/docs/leaderboards/"
  :type '(repeat string))

(defvar aidermacs--cached-models aidermacs-popular-models
  "Cache of available AI models.")

(defconst aidermacs--api-providers
  '(("https://openrouter.ai/api/v1" . ((hostname . "openrouter.ai")
                                       (prefix . "openrouter")
                                       (env-var . "OPENROUTER_API_KEY")
                                       (auth-header . (("Authorization" . "Bearer %s")))
                                       (models-path . "/models")
                                       (models-key . data)))
    ("https://api.openai.com/v1" . ((hostname . "api.openai.com")
                                    (prefix . "openai")
                                    (env-var . "OPENAI_API_KEY")
                                    (auth-header . (("Authorization" . "Bearer %s")))
                                    (models-path . "/models")
                                    (models-key . data)))
    ("https://api.deepseek.com" . ((hostname . "api.deepseek.com")
                                   (prefix . "deepseek")
                                   (env-var . "DEEPSEEK_API_KEY")
                                   (auth-header . (("Authorization" . "Bearer %s")))
                                   (models-path . "/models")
                                   (models-key . data)))
    ("https://api.anthropic.com/v1" . ((hostname . "api.anthropic.com")
                                       (prefix . "anthropic")
                                       (env-var . "ANTHROPIC_API_KEY")
                                       (auth-header . (("x-api-key" . "%s")
                                                       ("anthropic-version" . "2023-06-01")))
                                       (models-path . "/models")
                                       (models-key . data)))
    ("https://generativelanguage.googleapis.com/v1beta" . ((hostname . "generativelanguage.googleapis.com")
                                                           (prefix . "gemini")
                                                           (env-var . "GEMINI_API_KEY")
                                                           (auth-header . nil)
                                                           (models-path . "/models?key=%s")
                                                           (models-key . models)
                                                           (model-name-transform . (lambda (name)
                                                                                     (replace-regexp-in-string "^models/" "" name)))))
    ("https://api.x.ai/v1" . ((hostname . "api.x.ai")
                              (prefix . "xai")
                              (env-var . "XAI_API_KEY")
                              (auth-header . (("Authorization" . "Bearer %s")))
                              (models-path . "/models")
                              (models-key . data))))
  "Configuration for different API providers.
Each entry maps a base URL to a configuration alist with:
- hostname: The API hostname
- prefix: Prefix to add to model names
- env-var: Environment variable containing the API key
- auth-header: Headers for authentication (nil if not needed)
- models-path: Path to fetch models, with %s for token if needed
- models-key: JSON key containing the models list
- model-name-transform: Optional function to transform model names")

(defun aidermacs--fetch-openai-compatible-models (url token)
  "Fetch available models from an OpenAI compatible API endpoint.
URL should be the base API endpoint, e.g. https://api.openai.com/v1.
TOKEN is the API token for authentication.
Returns a list of model names with appropriate prefixes based on the
API provider."
  (let* ((provider-config (cdr (assoc url aidermacs--api-providers)))
         (prefix (alist-get 'prefix provider-config))
         (auth-headers (alist-get 'auth-header provider-config))
         (models-path (alist-get 'models-path provider-config))
         (models-key (alist-get 'models-key provider-config))
         (transform-fn (alist-get 'model-name-transform provider-config)))

    (unless provider-config
      (error "Unknown API URL: %s" url))

    (with-local-quit
      (with-current-buffer
          (let ((url-request-extra-headers
                 (when auth-headers
                   (mapcar (lambda (header)
                             (cons (car header)
                                   (format (cdr header) token)))
                           auth-headers))))
            (url-retrieve-synchronously
             (concat url (if (string-match-p "%s" models-path)
                             (format models-path token)
                           models-path))))

        (goto-char url-http-end-of-headers)
        (let* ((json-object-type 'alist)
               (json-data (json-read))
               (models (alist-get models-key json-data)))
          (mapcar (lambda (model)
                    (concat prefix "/"
                            (cond
                             (transform-fn (funcall transform-fn (alist-get 'name model)))
                             ((stringp model) model)
                             (t (or (alist-get 'id model)
                                    (alist-get 'name model))))))
                  models))))))

(defun aidermacs--select-model ()
  "Provide model selection with completion.
This is a private function used internally."
  (condition-case nil
      (let ((model (completing-read "Select AI model: " aidermacs--cached-models nil t)))
        (when model
          (aidermacs--send-command (format "/model %s" model))))
    (quit (message "Model selection cancelled"))))

(defun aidermacs--get-available-models ()
  "Get list of models supported by aider using the /models command.
This fetches models from various API providers and caches them."
  (aidermacs--send-command
   "/models /" nil nil t
   (lambda ()
     (let* ((supported-models
             (seq-filter
              (lambda (line)
                (string-prefix-p "- " line))
              (split-string aidermacs--current-output "\n" t)))
            (models nil))
       (setq supported-models
             (mapcar (lambda (line)
                       (substring line 2)) ; Remove "- " prefix
                     supported-models))
       (dolist (provider-entry aidermacs--api-providers)
         (let* ((url (car provider-entry))
                (config (cdr provider-entry))
                (env-var (alist-get 'env-var config))
                (token-value (getenv env-var)))
           (when (and token-value (not (string-empty-p token-value)))
             (condition-case err
                 (let* ((fetched-models (aidermacs--fetch-openai-compatible-models url token-value))
                        (filtered-models (seq-filter (lambda (model)
                                                       (member model supported-models))
                                                     fetched-models)))
                   (setq models (append models filtered-models)))
               (error (message "Failed to fetch models from %s: %s" url (error-message-string err)))))))
       (setq aidermacs--cached-models models)
       (aidermacs--select-model)))))

(defun aidermacs-clear-model-cache ()
  "Clear the cached models, forcing a fresh fetch on next use.
This is useful when available models have changed."
  (interactive)
  (setq aidermacs--cached-models nil)
  (message "Model cache cleared"))

;;;###autoload
(defun aidermacs-change-model ()
  "Interactively select and change AI model in current aidermacs session."
  (interactive)
  (when (and aidermacs--cached-models
             (equal aidermacs--cached-models aidermacs-popular-models)
             (fboundp 'aidermacs-get-buffer-name)
             (get-buffer (aidermacs-get-buffer-name)))
    (setq aidermacs--cached-models nil))

  (if aidermacs--cached-models
      (aidermacs--select-model)
    (aidermacs--get-available-models)))

(provide 'aidermacs-models)
;;; aidermacs-models.el ends here
