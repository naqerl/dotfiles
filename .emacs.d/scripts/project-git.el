;;;; project-git --- Make project and git be friends ;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Create new projects with ease

(require 'my-extensions)
(require 'project-persp)

(defvar project-git--clone-from nil
  "Source to clone from.")

(defconst project-git-base-path (expand-file-name "~/code/")
  "Base path to clone to.")

;;; Code:
(defun project-git-clone ()
  "Clones project to the given directory."
  (interactive)
  (let* ((default-directory
          (read-directory-name "Base directory: "
                               project-git-base-path))
         (project-git--clone-from (read-string "Clone from: "))
         (project-dir-name
          (car
           (my/track-new-directories
            '(shell-command
              (format "git clone %s" project-git--clone-from)))))
         (project-path (expand-file-name project-dir-name)))
    (project-persp-discover project-path 1)
    (project-persp-switch project-path)))

(provide 'project-git)
;;; project-git.el ends here
