;; Author: Mingde (Matthew) Zeng <matthewzmd@posteo.net>
;; Version: 0.5.0
;; Package-Requires: ((emacs "26.1") (transient "0.3.0"))
;; Keywords: ai emacs agents llm aider ai-pair-programming, convenience, tools
;; URL: https://github.com/MatthewZMD/aidermacs.el
;; Originally inspired by: Kang Tu <tninja@gmail.com> Aider.el

;; New function to run `find-name-dired` from the Git repository root directory
;;;###autoload
(defun aidermacs-repo-find-name-dired (pattern)
  "Run `find-name-dired` from the Git repository root directory with the given PATTERN."
  (interactive "sFind name (pattern): ")
  (let* ((git-repo-path (shell-command-to-string "git rev-parse --show-toplevel"))
         (repo-path (string-trim git-repo-path)))
    (if (string-match-p "fatal" repo-path)
        (message "Not in a git repository")
      (find-name-dired repo-path pattern))))

;;;###autoload
(defun aidermacs-git-repo-root-dired ()
  "Open a Dired buffer at the root of the current Git repository."
  (interactive)
  (let ((git-repo-path (shell-command-to-string "git rev-parse --show-toplevel")))
    (if (string-match-p "fatal" git-repo-path)
        (message "The current buffer is not in a Git repository.")
      (let ((repo-path (string-trim git-repo-path)))
        (dired-other-window repo-path)))))

(provide 'aidermacs-utils)
