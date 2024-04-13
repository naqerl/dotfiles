(require 'project)
(require 'suzu-extensions)

(defun suzu/project--get-last-two-elements (dir)
  "Get the last two elements of a path."
  (let* ((dir-components (split-string dir "\/" t))
         (last-two (last dir-components 2))
         (result
          (if (string-match-p "\\(~\\|suzu\\).*" (car last-two))
              (last last-two 1)
            last-two)))
    (mapconcat 'identity result "/")))

(defun suzu/project-switch-in-new-perspective ()
  "Opens project in a new perspective"
  (interactive)
  (let* ((project-dir (project-prompt-project-dir))
         (persp-name
          (suzu/project--get-last-two-elements project-dir)))
    (persp-switch persp-name)
    (message project-dir)
    (project-switch-project project-dir)))

(defun suzu/project-discover-in-directory (directory &optional depth)
  "Recursively searches projects under given directroy.
   Default depth is 6
   Returns number of total found projects"
  (interactive (list (read-directory-name "Base search path: ")))
  (or depth (setq depth 6))

  (when (not (file-directory-p directory))
    (error "Base path should be a directory."))

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
                       (suzu/project-discover-in-directory
                        dir
                        (1- depth))))))))
    (message "Total projects found: %s" projects-found)
    projects-found))

(provide 'suzu-project)
