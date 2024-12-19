;;;; project-new --- Handy ways to create new project ;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Create new projects with ease

(require 'my-extensions)
(require 'project)

(defvar project-new-created-hook nil
  "Hook that runs after project creation.
PROJECT-PATH provided as argument")

(defvar project-new--git-clone-from nil
  "Source to clone from.")

(defvar project-new--name nil
  "Name for the new project.")

(defconst project-git-base-path (expand-file-name "~/code/")
  "Base path to clone to.")

;;; Code:
(defun project-new-git-clone ()
  "Clones project to the given directory."
  (interactive)
  (let* ((default-directory
          (read-directory-name "Base directory: "
                               project-git-base-path))
         (project-new--git-clone-from (read-string "Clone from: "))
         (project-dir-name
          (car
           (my/track-new-directories
            '(shell-command
              (format "git clone %s" project-new--git-clone-from)))))
         (project-path
          (expand-file-name (format "%s/" project-dir-name))))
    (project-new--after-created project-path)))

(defun project-new-custom ()
  "Creates new project with custom command."
  (interactive)
  (let* ((default-directory
          (read-directory-name "Project path: "
                               project-git-base-path))
         (cmd (read-string "Command: ")))
    (unless (file-exists-p default-directory)
      (make-directory default-directory t))
    (with-temp-buffer
      (shell-command cmd t t))
    (project-new--after-created default-directory)))

(defun project-new--after-created (project-path)
  "Call after project created and pass PROJECT-PATH."
  (message "After project created at %s" project-path)
  (project-remember-projects-under project-path)
  (run-hook-with-args 'project-new-created-hook project-path))

(provide 'project-new)
;;; project-new.el ends here
