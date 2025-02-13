;;; aidermacs.el --- aidermacs package for interactive conversation with aidermacs -*- lexical-binding: t; -*-a

;; Author: Mingde (Matthew) Zeng <matthewzmd@posteo.net>
;; Original Author: Kang Tu <tninja@gmail.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "26.1") (transient "0.3.0"))
;; Keywords: convenience, tools
;; URL: https://github.com/MatthewZMD/aidermacs.el

;;; Commentary:
;; This package provides an interactive interface to communicate with https://github.com/paul-gauthier/aidermacs.

;;; Code:

(require 'comint)
(require 'dired)
(require 'transient)
(require 'vc-git)
(require 'which-func)
(require 'ansi-color)


(require 'aidermacs-backends)
(require 'aidermacs-models)
(when (featurep 'doom)
  (require 'aidermacs-doom))

(defgroup aidermacs nil
  "Customization group for the aidermacs package."
  :prefix "aidermacs-"
  :group 'convenience)

(defcustom aidermacs-program "aider"
  "The name or path of the aidermacs program."
  :type 'string
  :group 'aidermacs)

(defcustom aidermacs-args '("--model" "anthropic/claude-3-5-sonnet-20241022")
  "Arguments to pass to the aidermacs command."
  :type '(repeat string)
  :group 'aidermacs)

(defcustom aidermacs-subtree-only nil
  "When non-nil, run aider with --subtree-only flag to only consider files in current directory.
This is useful for working in monorepos where you want to limit aider's scope."
  :type 'boolean
  :group 'aidermacs)

(defcustom aidermacs-auto-commits t
  "When non-nil, enable auto-commits of LLM changes.
When nil, disable auto-commits requiring manual git commits."
  :type 'boolean
  :group 'aidermacs)

(defun aidermacs-project-root ()
  "Get the project root using project.el, VC, or fallback to file directory."
  (or (when (fboundp 'project-current)
        (project-root (project-current)))
      (vc-git-root default-directory)
      (when buffer-file-name
        (file-name-directory buffer-file-name))
      default-directory))

(defcustom aidermacs--switch-to-buffer-other-frame nil
  "When non-nil, open aidermacs buffer in a new frame using `switch-to-buffer-other-frame'.
When nil, use standard `display-buffer' behavior."
  :type 'boolean
  :group 'aidermacs)


(defcustom aidermacs-language-name-map '(("elisp" . "emacs-lisp")
                                         ("bash" . "sh")
                                         ("objective-c" . "objc")
                                         ("objectivec" . "objc")
                                         ("cpp" . "c++"))
  "Map external language names to Emacs names."
  :type '(alist :key-type (string :tag "Language Name/Alias")
                :value-type (string :tag "Mode Name (without -mode)"))
  :group 'aidermacs)

(defcustom aidermacs-prompt-file-name ".aider.prompt.org"
  "File name that will automatically enable aidermacs-minor-mode when opened.
This is the file name without path."
  :type 'string
  :group 'aidermacs)

(defvar aidermacs-read-string-history nil
  "History list for aidermacs read string inputs.")

(if (bound-and-true-p savehist-loaded)
    (add-to-list 'savehist-additional-variables 'aidermacs-read-string-history)
  (add-hook 'savehist-mode-hook
            (lambda ()
              (add-to-list 'savehist-additional-variables 'aidermacs-read-string-history))))


;;;###autoload
(defun aidermacs-plain-read-string (prompt &optional initial-input)
  "Read a string from the user with PROMPT and optional INITIAL-INPUT.
This function can be customized or redefined by the user."
  (read-string prompt initial-input 'aidermacs-read-string-history))

;;;###autoload
(defalias 'aidermacs-read-string 'aidermacs-plain-read-string)

(eval-and-compile
  ;; Ensure the alias is always available in both compiled and interpreted modes.
  (defalias 'aidermacs-read-string 'aidermacs-plain-read-string))

(defvar aidermacs--add-file-read-only nil
  "Set model parameters from `aidermacs-menu' buffer-locally.
Affects the system message too.")

(defun aidermacs--get-add-command-prefix ()
  "Return the appropriate command prefix based on aidermacs--add-file-read-only."
  (if aidermacs--add-file-read-only "/read-only" "/add"))

(defclass aidermacs--add-file-type (transient-lisp-variable)
  ((variable :initform 'aidermacs--add-file-read-only)
   (format :initform "%k %d %v")
   (reader :initform #'transient-lisp-variable--read-value))
  "Class for toggling aidermacs--add-file-read-only.")

(defclass aidermacs--switch-to-buffer-type (transient-lisp-variable)
  ((variable :initform 'aidermacs--switch-to-buffer-other-frame)
   (format :initform "%k %d %v")
   (reader :initform #'transient-lisp-variable--read-value))
  "Class for toggling aidermacs--switch-to-buffer-other-frame.")

(transient-define-infix aidermacs--infix-add-file-read-only ()
  "Toggle aidermacs--add-file-read-only between nil and t."
  :class 'aidermacs--add-file-type
  :key "@"
  :description "Read-only mode"
  :reader (lambda (_prompt _initial-input _history)
            (not aidermacs--add-file-read-only)))

(transient-define-infix aidermacs--infix-switch-to-buffer-other-frame ()
  "Toggle aidermacs--switch-to-buffer-other-frame between nil and t."
  :class 'aidermacs--switch-to-buffer-type
  :key "^"
  :description "Open in new frame"
  :reader (lambda (_prompt _initial-input _history)
            (not aidermacs--switch-to-buffer-other-frame)))

;; Transient menu for aidermacs commands
;; The instruction in the autoload comment is needed, see
;; https://github.com/magit/transient/issues/280.
;;;###autoload (autoload 'aidermacs-transient-menu "aidermacs" "Transient menu for aidermacs commands." t)
(transient-define-prefix aidermacs-transient-menu ()
  "Transient menu for aidermacs commands."
  ["aidermacs: AI Pair Programming"
   ["Session Control"
    (aidermacs--infix-switch-to-buffer-other-frame)
    ("a" "Start/Open Aidermacs"       aidermacs-run-aidermacs)
    ("." "Run in Current Dir"         aidermacs-run-in-current-dir)
    ("z" "Switch to Buffer"           aidermacs-switch-to-buffer)
    ("o" "Select Model"               aidermacs-change-model)
    ("l" "Clear Session"              aidermacs-clear)
    ("s" "Reset Session"              aidermacs-reset)
    ("x" "Exit Session"               aidermacs-exit)]

   ["File Management"
    (aidermacs--infix-add-file-read-only)
    ("f" "Add Current File"           aidermacs-add-current-file)
    ("R" "Add File Read-Only"         aidermacs-current-file-read-only)
    ("w" "Add Files in Window"        aidermacs-add-files-in-current-window)
    ("d" "Add Files by Type"          aidermacs-add-same-type-files-under-dir)
    ("b" "Add Marked Files"           aidermacs-batch-add-dired-marked-files)
    ("L" "List Added Files"           aidermacs-list-added-files)
    ("D" "Drop File from Chat"        aidermacs-drop-file)]

   ["Code Actions"
    ("c" "Code Change"                aidermacs-code-change)
    ("r" "Refactor Code"              aidermacs-function-or-region-refactor)
    ("i" "Implement TODO"             aidermacs-implement-todo)
    ("t" "Architect Discussion"       aidermacs-architect-discussion)
    ("u" "Undo Last Change"           aidermacs-undo-last-change)]

   ["Testing"
    ("U" "Write Unit Test"            aidermacs-write-unit-test)
    ("T" "Fix Failing Test"           aidermacs-fix-failing-test-under-cursor)
    ("X" "Debug Exception"            aidermacs-debug-exception)]

   ["Help & Documentation"
    ("q" "Ask Question"               aidermacs-ask-question)
    ("e" "Explain Code"               aidermacs-function-or-region-explain)
    ("p" "Explain Symbol"             aidermacs-explain-symbol-under-point)
    ("h" "Get Help"                   aidermacs-help)
    ("Q" "General Question"           aidermacs-general-question)]

   ["History & Output"
    ("H" "Show Output History"        aidermacs-show-output-history)
    ("C" "Copy Last Output"           aidermacs-get-last-output)
    ("m" "Show Last Commit"           aidermacs-magit-show-last-commit)
    ("y" "Go Ahead"                   aidermacs-go-ahead)
    ("g" "General Command"            aidermacs-general-command)
    ("P" "Open Prompt File"           aidermacs-open-prompt-file)]])

(defun aidermacs-buffer-name ()
  "Generate the aidermacs buffer name based on the git repo or current buffer file path.
If not in a git repository and no buffer file exists, an error is raised."
  (let ((git-repo-path (vc-git-root default-directory))
        (current-file (buffer-file-name)))
    (cond
     ;; Case 1: Valid git repo path
     (git-repo-path
      (format "*aidermacs:%s*" (file-truename git-repo-path)))
     ;; Case 2: Has buffer file
     (current-file
      (format "*aidermacs:%s*"
              (file-truename (file-name-directory current-file))))
     ;; Case 3: No git repo and no buffer file
     (t
      (error "Not in a git repository and current buffer is not associated with a file.  Please open a file or start aidermacs from within a git repository.")))))

;;;###autoload
(defun aidermacs-run-aidermacs (&optional edit-args directory)
  "Run aidermacs process using the selected backend.
With the universal argument EDIT-ARGS, prompt to edit aidermacs-args before running.
Optional DIRECTORY parameter sets working directory, defaults to project root."
  (interactive "P")
  (let* ((default-directory (or directory (aidermacs-project-root)))
         (buffer-name (aidermacs-buffer-name))
         (current-args (if edit-args
                           (split-string (read-string "Edit aidermacs arguments: "
                                                      (mapconcat 'identity aidermacs-args " ")))
                         aidermacs-args))
         (final-args (append current-args
                             (when (not aidermacs-auto-commits)
                               '("--no-auto-commits"))
                             (when aidermacs-subtree-only
                               '("--subtree-only")))))
    (if (get-buffer buffer-name)
        (aidermacs-switch-to-buffer)
      (aidermacs-run-aidermacs-backend aidermacs-program final-args buffer-name)
      (aidermacs-switch-to-buffer))))

;;;###autoload
(defun aidermacs-run-in-current-dir ()
  "Run aidermacs in the current directory with --subtree-only flag.
This is useful for working in monorepos where you want to limit aider's scope."
  (interactive)
  (let ((aidermacs-subtree-only t))
    (aidermacs-run-aidermacs nil default-directory)))

(defun aidermacs--send-command (command &optional switch-to-buffer callback)
  "Send COMMAND to the corresponding aidermacs process.
If SWITCH-TO-BUFFER is non-nil, switch to the aidermacs buffer.
If CALLBACK is provided, it will be called with the command output when available."
  (if-let ((aidermacs-buffer (get-buffer (aidermacs-buffer-name))))
      (let ((processed-command (aidermacs--process-message-if-multi-line command)))
        (when (and switch-to-buffer aidermacs-buffer)
          (aidermacs-switch-to-buffer))
        (aidermacs--send-command-backend aidermacs-buffer processed-command callback)
        (when (and switch-to-buffer (not (string= (buffer-name) (aidermacs-buffer-name))))
          (aidermacs-switch-to-buffer)))
    (message "Buffer %s does not exist. Please start aidermacs with 'M-x aidermacs-run-aidermacs'." aidermacs-buffer-name)))


;; Function to switch to the aidermacs buffer
;;;###autoload
(defun aidermacs-switch-to-buffer ()
  "Switch to the aidermacs buffer.
When `aidermacs--switch-to-buffer-other-frame' is non-nil, open in a new frame.
If the buffer is already visible in a window, switch to that window.
If the current buffer is already the aidermacs buffer, do nothing."
  (interactive)
  (let ((buffer (get-buffer (aidermacs-buffer-name))))
    (cond
     ((string= (buffer-name) (aidermacs-buffer-name)) t)
     ((and buffer (get-buffer-window buffer))
      (select-window (get-buffer-window buffer)))  ;; Switch to existing window
     (buffer
      (if aidermacs--switch-to-buffer-other-frame
          (switch-to-buffer-other-frame buffer)
        (pop-to-buffer buffer)))
     (t
      (message "Buffer '%s' does not exist." (aidermacs-buffer-name))))))

;; Function to reset the aidermacs buffer
;;;###autoload
(defun aidermacs-clear ()
  "Send the command \"/clear\" to the aidermacs buffer."
  (interactive)
  (aidermacs--send-command "/clear"))

;;;###autoload
(defun aidermacs-reset ()
  "Send the command \"/reset\" to the aidermacs buffer."
  (interactive)
  (aidermacs--send-command "/reset"))

;;;###autoload
(defun aidermacs-exit ()
  "Send the command \"/exit\" to the aidermacs buffer."
  (interactive)
  (aidermacs--send-command "/exit"))


(defun aidermacs--process-message-if-multi-line (str)
  "Entering multi-line chat messages
https://aidermacs.chat/docs/usage/commands.html#entering-multi-line-chat-messages
If STR contains newlines and isn't already wrapped in {aidermacs...aidermacs},
wrap it in {aidermacs\nstr\naidermacs}. Otherwise return STR unchanged."
  (if (and (string-match-p "\n" str)
           (not (string-match-p "^{aidermacs\n.*\naidermacs}$" str)))
      (format "{aidermacs\n%s\naidermacs}" str)
    str))

;;;###autoload
(defun aidermacs-add-or-read-current-file (command-prefix)
  "Send the command \"COMMAND-PREFIX <current buffer file full path>\" to the corresponding aidermacs comint buffer."
  ;; Ensure the current buffer is associated with a file
  (if (not buffer-file-name)
      (message "Current buffer is not associated with a file.")
    (let* ((file-path (buffer-file-name))
           ;; Use buffer-file-name directly
           (formatted-path (if (string-match-p " " file-path)
                               (format "\"%s\"" file-path)
                             file-path))
           (command (format "%s %s" command-prefix formatted-path)))
      ;; Use the shared helper function to send the command
      (aidermacs--send-command command))))

;; Function to send "/add <current buffer file full path>" to corresponding aidermacs buffer
;;;###autoload
(defun aidermacs-add-current-file ()
  "Send the command \"/add <current buffer file full path>\" to the corresponding aidermacs comint buffer."
  (interactive)
  (aidermacs-add-or-read-current-file (aidermacs--get-add-command-prefix)))

;;;###autoload
(defun aidermacs-current-file-read-only ()
  "Send the command \"/read-only <current buffer file full path>\" to the corresponding aidermacs comint buffer."
  (interactive)
  (aidermacs-add-or-read-current-file "/read-only"))

;; New function to add files in all buffers in current emacs window
;;;###autoload
(defun aidermacs-add-files-in-current-window ()
  "Add files in all buffers in the current Emacs window to the aidermacs buffer."
  (interactive)
  (let ((files (mapcar (lambda (buffer)
                         (with-current-buffer buffer
                           (when buffer-file-name
                             (expand-file-name buffer-file-name))))
                       (mapcar 'window-buffer (window-list)))))
    (setq files (delq nil files))
    (if files
        (let ((command (concat (aidermacs--get-add-command-prefix) " " (mapconcat 'identity files " "))))
          (aidermacs--send-command command nil))
      (message "No files found in the current window."))))

;; Function to send a custom command to corresponding aidermacs buffer
;;;###autoload
(defun aidermacs-general-command ()
  "Prompt the user to input COMMAND and send it to the corresponding aidermacs comint buffer."
  (interactive)
  (let ((command (aidermacs-read-string "Enter command to send to aidermacs: ")))
    ;; Use the shared helper function to send the command
    (aidermacs--send-command command t)))

;; New function to get command from user and send it prefixed with "/code "
;;;###autoload
(defun aidermacs-code-change ()
  "Prompt the user for a command and send it to the corresponding aidermacs comint buffer prefixed with \"/code \"."
  (interactive)
  (let ((command (aidermacs-read-string "Enter code change requirement: ")))
    (aidermacs-send-command-with-prefix "/code " command)))

(defun aidermacs--parse-ls-output (output)
  "Parse the /ls command OUTPUT to extract files in chat.
After the \"Files in chat:\" header, each subsequent line that begins with
whitespace is processed. The first non-whitespace token is taken as the file name.
Relative paths are resolved using the repository root (if available) or
`default-directory`. Only files that exist on disk are included in the result.
Returns a deduplicated list of such file names."
  (when output
    (with-temp-buffer
      (insert output)
      (goto-char (point-min))
      (when (search-forward "Files in chat:" nil t)
        ;; Skip header and any blank lines.
        (forward-line 1)
        (while (and (not (eobp))
                    (string-empty-p (string-trim (thing-at-point 'line t))))
          (forward-line 1))
        (let ((base (or (vc-git-root default-directory)
                        default-directory))
              files)
          ;; Process each line that begins with whitespace.
          (while (and (not (eobp))
                      (string-match-p "^[[:space:]]" (thing-at-point 'line t)))
            (let* ((line (string-trim (thing-at-point 'line t)))
                   (parts (split-string line))
                   (file (if parts (car parts) "")))
              (unless (string-empty-p file)
                (let ((file-abs (if (file-name-absolute-p file)
                                    file
                                  (expand-file-name file base))))
                  (when (file-exists-p file-abs)
                    (push file files)))))
            (forward-line 1))
          (delete-dups (nreverse files)))))))

;;;###autoload
(defun aidermacs-list-added-files ()
  "List all files currently added to the chat session.
Sends the \"/ls\" command and returns the list of files via callback."
  (interactive)
  (aidermacs--send-command
   "/ls" t
   (lambda (output)
     (let ((files (aidermacs--parse-ls-output output)))
       (message "%s" (prin1-to-string files))
       files))))

;;;###autoload
(defun aidermacs-drop-file ()
  "Drop a file from the chat session by selecting from currently added files."
  (interactive)
  (aidermacs--send-command
   "/ls" t
   (lambda (output)
     (condition-case nil
         (if-let* ((files (aidermacs--parse-ls-output output))
                   (file (completing-read "Select file to drop: " files nil t)))
             (progn
               (aidermacs--send-command (format "/drop %s" file)))
           (message "No files available to drop"))
       (quit (message "Drop file cancelled"))))))


;;;###autoload
(defun aidermacs-show-output-history ()
  "Display the AI output history in a new buffer."
  (interactive)
  (let ((buf (get-buffer-create "*aidermacs-history*"))
        (history aidermacs--output-history))
    (with-current-buffer buf
      (org-mode)
      (setq buffer-read-only nil)
      (erase-buffer)
      (display-line-numbers-mode 1)
      (dolist (entry history)
        (let ((timestamp (format-time-string "%Y-%m-%d %H:%M:%S" (car entry)))
              (output (cdr entry)))
          (insert (format "* %s\n#+BEGIN_SRC\n%s\n#+END_SRC\n" timestamp output))))
      (goto-char (point-min))
      (setq buffer-read-only t)
      (local-set-key (kbd "q") 'kill-this-buffer)
      (switch-to-buffer-other-frame buf))))

;;;###autoload
(defun aidermacs-get-last-output ()
  "Get the most recent output from aidermacs."
  (interactive)
  (message aidermacs--current-output)
  (kill-new aidermacs--current-output)
  aidermacs--current-output)


;; New function to get command from user and send it prefixed with "/ask "
;;;###autoload
(defun aidermacs-ask-question ()
  "Prompt the user for a command and send it to the corresponding aidermacs comint buffer prefixed with \"/ask \".
If a region is active, append the region text to the question.
If cursor is inside a function, include the function name as context."
  (interactive)
  ;; Dispatch to general question if in aidermacs buffer
  (when (string= (buffer-name) (aidermacs-buffer-name))
    (call-interactively 'aidermacs-general-question)
    (cl-return-from aidermacs-ask-question))

  (let* ((function-name (which-function))
         (initial-input (when function-name
                          (format "About function '%s': " function-name)))
         (question (aidermacs-read-string "Enter question to ask: " initial-input))
         (region-text (and (region-active-p)
                           (buffer-substring-no-properties (region-beginning) (region-end))))
         (command (if region-text
                      (format "/ask %s: %s" question region-text)
                    (format "/ask %s" question))))
    (aidermacs-add-current-file)
    (aidermacs--send-command command t)))

;;;###autoload
(defun aidermacs-general-question ()
  "Prompt the user for a general question and send it to the corresponding aidermacs comint buffer prefixed with \"/ask \"."
  (interactive)
  (let ((question (aidermacs-read-string "Enter general question to ask: ")))
    (let ((command (format "/ask %s" question)))
      (aidermacs--send-command command t))))

;; New function to get command from user and send it prefixed with "/help "
;;;###autoload
(defun aidermacs-help ()
  "Prompt the user for a command and send it to the corresponding aidermacs comint buffer prefixed with \"/help \"."
  (interactive)
  (let ((command (aidermacs-read-string "Enter help question: ")))
    (aidermacs-send-command-with-prefix "/help " command)))

;; New function to get command from user and send it prefixed with "/architect "
;;;###autoload
(defun aidermacs-architect-discussion ()
  "Prompt the user for a command and send it to the corresponding aidermacs comint buffer prefixed with \"/architect \"."
  (interactive)
  (let ((command (aidermacs-read-string "Enter architect discussion question: ")))
    (aidermacs-send-command-with-prefix "/architect " command)))

;; New function to get command from user and send it prefixed with "/ask ", might be tough for AI at this moment
;;;###autoload
(defun aidermacs-debug-exception ()
  "Prompt the user for a command and send it to the corresponding aidermacs comint buffer prefixed with \"/debug \",
replacing all newline characters except for the one at the end."
  (interactive)
  (let ((command (aidermacs-plain-read-string "Enter exception, can be multiple lines: ")))
    (aidermacs--send-command (concat "/ask Investigate the following exception, with current added files as context: " command) t)))

;;;###autoload
(defun aidermacs-go-ahead ()
  "Send the command \"go ahead\" to the corresponding aidermacs comint buffer."
  (interactive)
  (aidermacs--send-command "go ahead" t))

;; New function to show the last commit using magit
;;;###autoload
(defun aidermacs-magit-show-last-commit ()
  "Show the last commit message using Magit.
If Magit is not installed, report that it is required."
  (interactive)
  (if (require 'magit nil 'noerror)
      (magit-show-commit "HEAD")
    (message "Magit is required to show the last commit.")))

;; Modified function to get command from user and send it based on selected region
;;;###autoload
(defun aidermacs-undo-last-change ()
  "Undo the last change made by aidermacs."
  (interactive)
  (aidermacs--send-command "/undo"))

;;;###autoload
(defun aidermacs-function-or-region-refactor ()
  "Refactor code at point or region.
If region is active, refactor that region.
If point is in a function, refactor that function."
  (interactive)
  (if (use-region-p)
      (let* ((region-text (buffer-substring-no-properties (region-beginning) (region-end)))
             (function-name (which-function))
             (user-command (aidermacs-read-string "Enter refactor instruction: "))
             (command (if function-name
                          (format "/architect \"in function %s, for the following code block, %s: %s\"\n"
                                  function-name user-command region-text)
                        (format "/architect \"for the following code block, %s: %s\"\n"
                                user-command region-text))))
        (aidermacs-add-current-file)
        (aidermacs--send-command command t))
    (if-let ((function-name (which-function)))
        (let* ((initial-input (format "refactor %s: " function-name))
               (user-command (aidermacs-read-string "Enter refactor instruction: " initial-input))
               (command (format "/architect %s" user-command)))
          (aidermacs-add-current-file)
          (aidermacs--send-command command t))
      (message "No region selected and no function found at point."))))

;; New function to explain the code in the selected region
;;;###autoload
(defun aidermacs-region-explain ()
  "Get a command from the user and send it to the corresponding aidermacs comint buffer based on the selected region.
The command will be formatted as \"/ask \" followed by the text from the selected region."
  (interactive)
  (if (use-region-p)
      (let* ((region-text (buffer-substring-no-properties (region-beginning) (region-end)))
             (function-name (which-function))
             (processed-region-text region-text)
             (command (if function-name
                          (format "/ask in function %s, explain the following code block: %s"
                                  function-name
                                  processed-region-text)
                        (format "/ask explain the following code block: %s"
                                processed-region-text))))
        (aidermacs-add-current-file)
        (aidermacs--send-command command t))
    (message "No region selected.")))

;; New function to ask aidermacs to explain the function under the cursor
;;;###autoload
(defun aidermacs-function-explain ()
  "Ask aidermacs to explain the function under the cursor.
Prompts user for specific questions about the function."
  (interactive)
  (if-let ((function-name (which-function)))
      (let* ((initial-input (format "explain %s: " function-name))
             (user-question (aidermacs-read-string "Enter your question about the function: " initial-input))
             (command (format "/ask %s" user-question)))
        (aidermacs-add-current-file)
        (aidermacs--send-command command t))
    (message "No function found at cursor position.")))

;;;###autoload
(defun aidermacs-function-or-region-explain ()
  "Call aidermacs-function-explain when no region is selected, otherwise call aidermacs-region-explain."
  (interactive)
  (if (region-active-p)
      (aidermacs-region-explain)
    (aidermacs-function-explain)))

;; New function to explain the symbol at line
;;;###autoload
(defun aidermacs-explain-symbol-under-point ()
  "Ask aidermacs to explain symbol under point, given the code line as background info."
  (interactive)
  (let* ((symbol (thing-at-point 'symbol))
         (line (buffer-substring-no-properties
                (line-beginning-position)
                (line-end-position)))
         (prompt (format "/ask Please explain what '%s' means in the context of this code line: %s"
                         symbol line)))
    (aidermacs-add-current-file)
    (aidermacs--send-command prompt t)))

(defun aidermacs-send-command-with-prefix (prefix command)
  "Send COMMAND to the aidermacs buffer prefixed with PREFIX."
  (aidermacs-add-current-file)
  (aidermacs--send-command (concat prefix command) t))

;;; functions for dired related

;; New function to add multiple Dired marked files to aidermacs buffer
;;;###autoload
(defun aidermacs-batch-add-dired-marked-files ()
  "Add multiple Dired marked files to the aidermacs buffer with the \"/add\" command."
  (interactive)
  (let ((files (dired-get-marked-files)))
    (if files
        (let ((command (concat (aidermacs--get-add-command-prefix) " " (mapconcat 'expand-file-name files " "))))
          (aidermacs--send-command command t))
      (message "No files marked in Dired."))))

;; New function to add all files with same suffix as current file under current directory
;;;###autoload
(defun aidermacs-add-same-type-files-under-dir ()
  "Add all files with same suffix as current file under current directory to aidermacs.
If there are more than 40 files, refuse to add and show warning message."
  (interactive)
  (if (not buffer-file-name)
      (message "Current buffer is not visiting a file")
    (let* ((current-suffix (file-name-extension buffer-file-name))
           (dir (file-name-directory buffer-file-name))
           (max-files 40)
           (files (directory-files dir t
                                   (concat "\\." current-suffix "$")
                                   t))) ; t means don't include . and ..
      (if (> (length files) max-files)
          (message "Too many files (%d, > %d) found with suffix .%s. Aborting."
                   (length files) max-files current-suffix)
        (let ((command (concat (aidermacs--get-add-command-prefix) " " (mapconcat 'identity files " "))))
          (aidermacs--send-command command t))
        (message "Added %d files with suffix .%s"
                 (length files) current-suffix)))))

;;; functions for test fixing

;;;###autoload
(defun aidermacs-write-unit-test ()
  "Generate unit test code for current buffer.
Do nothing if current buffer is not visiting a file.
If current buffer filename contains 'test':
  - If cursor is inside a test function, implement that test
  - Otherwise show message asking to place cursor inside a test function
Otherwise:
  - If cursor is on a function, generate unit test for that function
  - Otherwise generate unit tests for the entire file"
  (interactive)
  (if (not buffer-file-name)
      (message "Current buffer is not visiting a file.")
    (let ((is-test-file (string-match-p "test" (file-name-nondirectory buffer-file-name)))
          (function-name (which-function)))
      (cond
       ;; Test file case
       (is-test-file
        (if function-name
            (if (string-match-p "test" function-name)
                (let* ((initial-input
                        (format "Please implement test function '%s'. Follow standard unit testing practices and make it a meaningful test. Do not use Mock if possible."
                                function-name))
                       (user-command (aidermacs-read-string "Test implementation instruction: " initial-input))
                       (command (format "/architect %s" user-command)))
                  (aidermacs-add-current-file)
                  (aidermacs--send-command command t))
              (message "Current function '%s' does not appear to be a test function." function-name))
          (message "Please place cursor inside a test function to implement.")))
       ;; Non-test file case
       (t
        (let* ((common-instructions "Keep existing tests if there are. Follow standard unit testing practices. Do not use Mock if possible.")
               (initial-input
                (if function-name
                    (format "Please write unit test code for function '%s'. %s"
                            function-name common-instructions)
                  (format "Please write unit test code for file '%s'. For each function %s"
                          (file-name-nondirectory buffer-file-name) common-instructions)))
               (user-command (aidermacs-read-string "Unit test generation instruction: " initial-input))
               (command (format "/architect %s" user-command)))
          (aidermacs-add-current-file)
          (aidermacs--send-command command t)))))))

;;;###autoload
(defun aidermacs-fix-failing-test-under-cursor ()
  "Report the current test failure to aidermacs and ask it to fix the code.
This function assumes the cursor is on or inside a test function."
  (interactive)
  (if-let ((test-function-name (which-function)))
      (let* ((initial-input (format "The test '%s' is failing. Please analyze and fix the code to make the test pass. Don't break any other test"
                                    test-function-name))
             (test-output (aidermacs-read-string "Architect question: " initial-input))
             (command (format "/architect %s" test-output)))
        (aidermacs-add-current-file)
        (aidermacs--send-command command t))
    (message "No test function found at cursor position.")))

(defun aidermacs--is-comment-line (line)
  "Check if LINE is a comment line based on current buffer's comment syntax.
Returns non-nil if LINE starts with one or more comment characters,
ignoring leading whitespace."
  (when comment-start
    (let ((comment-str (string-trim-right comment-start)))
      (string-match-p (concat "^[ \t]*"
                              (regexp-quote comment-str)
                              "+")
                      (string-trim-left line)))))

;;;###autoload
(defun aidermacs-implement-todo ()
  "Implement TODO comments in current context.
If region is selected, implement that specific region.
If cursor is on a comment line, implement that specific comment.
If cursor is inside a function, implement TODOs for that function.
Otherwise implement TODOs for the entire current file."
  (interactive)
  (if (not buffer-file-name)
      (message "Current buffer is not visiting a file.")
    (let* ((current-line (string-trim (thing-at-point 'line t)))
           (is-comment (aidermacs--is-comment-line current-line))
           (function-name (which-function))
           (region-text (when (region-active-p)
                          (buffer-substring-no-properties
                           (region-beginning)
                           (region-end))))
           (initial-input
            (cond
             (region-text
              (format "Please implement this code block: '%s'. It is already inside current code. Please do in-place implementation. Keep the existing code structure and implement just this specific block."
                      region-text))
             (is-comment
              (format "Please implement this comment: '%s'. It is already inside current code. Please do in-place implementation. Keep the existing code structure and implement just this specific comment."
                      current-line))
             (function-name
              (format "Please implement the TODO items in function '%s'. Keep the existing code structure and only implement the TODOs in comments."
                      function-name))
             (t
              (format "Please implement all TODO items in file '%s'. Keep the existing code structure and only implement the TODOs in comments."
                      (file-name-nondirectory buffer-file-name)))))
           (user-command (aidermacs-read-string "TODO implementation instruction: " initial-input))
           (command (format "/architect %s" user-command)))
      (aidermacs-add-current-file)
      (aidermacs--send-command command t))))


;;; functions for sending text blocks

;; New function to send "<line under cursor>" or region line by line to the aidermacs buffer
;;;###autoload
(defun aidermacs-send-line-or-region ()
  "Send text to the aidermacs buffer.
If region is active, send the selected region line by line.
Otherwise, send the line under cursor."
  (interactive)
  (if (region-active-p)
      (aidermacs-send-region-by-line)
    (let ((line (thing-at-point 'line t)))
      (aidermacs--send-command (string-trim line) t))))

;;; New function to send the current selected region line by line to the aidermacs buffer
;;;###autoload
(defun aidermacs-send-region-by-line ()
  "Get the text of the current selected region, split them into lines,
strip the newline character from each line,
for each non-empty line, send it to aidermacs session.
If no region is selected, show a message."
  (interactive)
  (if (region-active-p)
      (let ((region-text (buffer-substring-no-properties
                          (region-beginning)
                          (region-end))))
        (mapc (lambda (line)
                (unless (string-empty-p line)
                  (aidermacs--send-command line t)))
              (split-string region-text "\n" t)))
    (message "No region selected.")))

;;;###autoload
(defun aidermacs-send-block-or-region ()
  "Send the current active region text or, if no region is active, send the current paragraph content to the aidermacs session.
When sending paragraph content, preserve cursor position and deactivate mark afterwards."
  (interactive)
  (if (region-active-p)
      (let ((region-text (buffer-substring-no-properties (region-beginning) (region-end))))
        (unless (string-empty-p region-text)
          (aidermacs--send-command region-text t)))
    (save-excursion  ; preserve cursor position
      (let ((region-text
             (progn
               (mark-paragraph)  ; mark paragraph
               (buffer-substring-no-properties (region-beginning) (region-end)))))
        (unless (string-empty-p region-text)
          (aidermacs--send-command region-text t))
        (deactivate-mark)))))  ; deactivate mark after sending

;;;###autoload
(defun aidermacs-open-prompt-file ()
  "Open aidermacs prompt file under git repo root.
If file doesn't exist, create it with command binding help and sample prompt."
  (interactive)
  (let* ((git-root (vc-git-root default-directory))
         (prompt-file (when git-root
                        (expand-file-name aidermacs-prompt-file-name git-root))))
    (if prompt-file
        (progn
          (find-file-other-window prompt-file)
          (unless (file-exists-p prompt-file)
            ;; Insert initial content for new file
            (insert "# aidermacs Prompt File - Command Reference:\n")
            (insert "# C-c C-n or C-<return>: Send current line or selected region line by line\n")
            (insert "# C-c C-c: Send current block or selected region as a whole\n")
            (insert "# C-c C-z: Switch to aidermacs buffer\n\n")
            (insert "* Sample task:\n\n")
            (insert "/ask what this repo is about?\n")
            (save-buffer)))
      (message "Not in a git repository"))))

;; Define the keymap for aidermacs Minor Mode
(defvar aidermacs-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-n") 'aidermacs-send-line-or-region)
    (define-key map (kbd "C-<return>") 'aidermacs-send-line-or-region)
    (define-key map (kbd "C-c C-c") 'aidermacs-send-block-or-region)
    (define-key map (kbd "C-c C-z") 'aidermacs-switch-to-buffer)
    map)
  "Keymap for aidermacs Minor Mode.")

;; Define the aidermacs Minor Mode
;;;###autoload
(define-minor-mode aidermacs-minor-mode
  "Minor mode for aidermacs with keybindings."
  :lighter " aidermacs"
  :keymap aidermacs-minor-mode-map
  :override t)

;; Auto-enable aidermacs-minor-mode for specific files
(defcustom aidermacs-auto-mode-files
  (list
   aidermacs-prompt-file-name    ; Default prompt file
   ".aider.chat.md"
   ".aider.chat.history.md"
   ".aider.input.history")
  "List of filenames that should automatically enable `aidermacs-minor-mode'.
These are exact filename matches (including the dot prefix)."
  :type '(repeat string)
  :group 'aidermacs)

(defun aidermacs--should-enable-minor-mode-p (filename)
  "Determine if aidermacs-minor-mode should be enabled for FILENAME.
Returns t if the file matches any of the patterns in `aidermacs-auto-mode-files'."
  (when filename
    (let ((base-name (file-name-nondirectory filename)))
      (member base-name aidermacs-auto-mode-files))))

(add-hook 'find-file-hook
          (lambda ()
            (when (and buffer-file-name
                       (aidermacs--should-enable-minor-mode-p buffer-file-name))
              (aidermacs-minor-mode 1))))

(provide 'aidermacs)

;;; aidermacs.el ends here
