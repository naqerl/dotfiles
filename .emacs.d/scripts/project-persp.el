;;; project-persp --- Make perspective and project be friends ;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Simple functions to add interations between perspective and project

;;; Code:
(require 'project)
(require 'my-extensions)
(require 'perspective)

(defun project-persp-compilation-buffer-name-function (_)
  "Creates unique compilation buffer name based on the current perspective."
  (concat "*" (project-name (project-current)) "/compile*"))

(defun project-persp-switch (&optional project-dir)
  "Opens project in a new perspective.
If PROJECT-DIR not specified then prompts for it"
  (interactive)
  (let ((project-dir (or project-dir (project-prompt-project-dir))))
    (project-persp--switch project-dir)))

(defun project-persp--switch (project-dir)
  "Creates new perspective for the given PROJECT-DIR."
  (persp-switch (project-persp--get-last-two-elements project-dir))
  (message project-dir)
  (setq-local project-current-directory-override project-dir)
  (project-find-file))

(defun project-persp--get-last-two-elements (dir)
  "Get the last two elements of a DIR."
  (let* ((dir-components (split-string dir "\/" t))
         (last-two (last dir-components 2))
         (result
          (if (string-match-p "\\(~\\|suzu\\).*" (car last-two))
              (last last-two 1)
            last-two)))
    (mapconcat 'identity result "/")))

(defun project-persp-discover (directory &optional depth)
  "Recursively searches projects under given DIRECTORY.
Default DEPTH is 6
Returns number of total found projects"
  (interactive (list (read-directory-name "Base search path: ")))
  (or depth (setq depth 6))
  (when (not (file-directory-p directory))
    (error "Base path should be a directory"))
  (message "Searching projects in %s" directory)
  (let ((projects-found (project-remember-projects-under directory)))
    (when (= projects-found 0)
      (if (and (numberp depth) (> depth 0))
          (dolist (dir
                   (ignore-errors
                     (directory-files directory
                                      t
                                      "^\\([^.]\\|\\.\\..\\)")))
            (when (file-directory-p dir)
              (setq projects-found
                    (+ projects-found
                       (project-persp-discover dir (1- depth))))))))
    (message "Total projects found: %s" projects-found)
    projects-found))

(provide 'project-persp)
;;; project-persp.el ends here
