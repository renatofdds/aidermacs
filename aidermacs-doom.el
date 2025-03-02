;;; aidermacs-doom.el --- Description -*- lexical-binding: t; no-byte-compile: t -*-
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
;; This file provides Doom Emacs integration for Aidermacs, offering
;; convenient keybindings and commands for interacting with Aider
;; within the Doom Emacs environment.
;;
;; Key features include:
;; - Keybindings for common Aidermacs actions, such as running Aider,
;;   adding files, and asking questions.
;; - Integration with Doom Emacs' keybinding system.
;; - Automatic setup of keybindings in Git repositories.
;;
;;; Code:

(defun aidermacs-doom-setup-keys ()
  "Setup aidermacs keybindings if the current buffer is in a git repository."
  (when (and (featurep 'doom-keybinds)
             (vc-backend (or (buffer-file-name) default-directory)))
    (map! :leader
          (:prefix ("A" . "aidermacs")
                   ;; Core Actions
                   :desc "Start/Open Session" "a" #'aidermacs-run
                   :desc "Start in Current Dir" "." #'aidermacs-run-in-current-dir
                   :desc "Change Solo Model" "o" #'aidermacs-change-model
                   :desc "Reset Session" "s" #'aidermacs-reset
                   :desc "Exit Session" "x" #'aidermacs-exit

                   ;; Quick Actions
                   :desc "Add Current File" "f" #'aidermacs-add-current-file
                   :desc "Code Change" "c" #'aidermacs-code-change
                   :desc "Refactor" "r" #'aidermacs-function-or-region-refactor
                   :desc "Go Ahead" "g" #'aidermacs-go-ahead

                   ;; File & Code
                   (:prefix ("F" . "File Commands")
                            :desc "File Commands" "F" #'aidermacs-transient-file-commands
                            ;; File Actions
                            :desc "Add Current File" "f" #'aidermacs-add-current-file
                            :desc "Add File Interactively" "i" #'aidermacs-add-files-interactively
                            :desc "Add Window Files" "w" #'aidermacs-add-files-in-current-window
                            :desc "Add Directory Files" "d" #'aidermacs-add-same-type-files-under-dir
                            :desc "Add Dired Marked" "m" #'aidermacs-batch-add-dired-marked-files
                            :desc "Drop File Interactively" "j" #'aidermacs-drop-file
                            :desc "Drop Current File" "k" #'aidermacs-drop-current-file
                            :desc "Drop All Files" "a" #'aidermacs-drop-all-files
                            :desc "List Files" "l" #'aidermacs-list-added-files)

                   (:prefix ("C" . "Code Commands")
                            :desc "Code Commands" "C" #'aidermacs-transient-code-commands
                            ;; Code Actions
                            :desc "Code Change" "c" #'aidermacs-code-change
                            :desc "Refactor Code" "r" #'aidermacs-function-or-region-refactor
                            :desc "Architect Discuss" "a" #'aidermacs-architect-discussion
                            :desc "Implement TODO" "i" #'aidermacs-implement-todo
                            :desc "Write Tests" "t" #'aidermacs-write-unit-test
                            :desc "Fix Test" "T" #'aidermacs-fix-failing-test-under-cursor
                            :desc "Debug Exception" "x" #'aidermacs-debug-exception
                            :desc "Undo Auto Git Commit" "u" #'aidermacs-undo-last-commit)

                   ;; Understanding
                   :desc "Show Last Commit" "m" #'aidermacs-magit-show-last-commit
                   :desc "Ask Question" "q" #'aidermacs-ask-question
                   :desc "Explain This Code" "e" #'aidermacs-explain-this-code
                   :desc "Explain This Symbol" "p" #'aidermacs-explain-symbol-under-point

                   ;; Others
                   :desc "Toggle Architect Mode (Separate Reasoner/Editor)" "A" #'aidermacs-toggle-architect-mode
                   :desc "Session History" "H" #'aidermacs-show-output-history
                   :desc "Copy Last Aidermacs Output" "L" #'aidermacs-get-last-output
                   :desc "Clear Model Selection Cache" "O" #'aidermacs-clear-model-cache
                   :desc "Clear Buffer" "l" #'aidermacs-clear
                   :desc "Aider Help" "h" #'aidermacs-help))))

;;;###autoload
(defun aidermacs-doom-enable ()
  "Enable Doom keybindings for aidermacs.
This adds the key setup function to relevant hooks.
Call this from your Doom config to enable the keybindings."
  (interactive)
  (add-hook 'find-file-hook #'aidermacs-doom-setup-keys)
  (add-hook 'dired-mode-hook #'aidermacs-doom-setup-keys)
  (add-hook 'after-change-major-mode-hook #'aidermacs-doom-setup-keys))

(provide 'aidermacs-doom)
;;; aidermacs-doom.el ends here
