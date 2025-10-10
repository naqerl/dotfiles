;;; llm-commit --- Generte git commit message based on the current stage ;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Automatically generates semantic commits for the stages changes
;;; Code:

(defconst llm-commit-prompt-template "You are an expert programmer writing a Git commit message.
You have carefully reviewed every file diff included in this commit.

First, choose the most appropriate label for the changes.
Here are the labels you can choose from:
- docs: Documentation-only changes (e.g., fixing typos, adding examples)
- feat: Introduces a new feature to the codebase
- fix: Patches a bug in the codebase
- refactor: Code changes that neither fix bugs nor add features
- test: Adds or corrects tests

Next, write a high-level summary of the commit.
- Keep it to a single line, no more than 50 characters
- Use the imperative tense (e.g., 'Add logging' not 'Added logging')
- Ensure the message reflects a clear and cohesive change
- Do not end the summary with a period
- Do not use backticks (`) anywhere in the response

THE FILE DIFFS:
```
%s
```
Now, write the commit message using this format: [label]: [summary]")

;;;###autoload
(defun llm-commit:generate ()
  "Insertes at point generated commit message."
  (interactive)
  (require 'gptel)
  (let ((default-directory (project-root (project-current))))
    (gptel-request (format
                    llm-commit-prompt-template
                    (shell-command-to-string "git diff --cached")))))

(provide 'llm-commit)
;;; llm-commit.el ends here
