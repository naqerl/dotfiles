(require 'project)

(defun suzu/get-last-two-elements (dir)
  "Get the last two elements of a path."
  (let* ((dir-components (split-string dir "\/" t))
         (last-two (last dir-components 2))
         (result (if (string-match-p "\\(~\\|suzu\\).*" (car last-two)) (last last-two 1) last-two)))
    (mapconcat 'identity result "/")))

(defun suzu/project-name-function (project-root)
  (suzu/get-last-two-elements project-root))

(defun suzu/project-switch-in-new-perspective ()
  (interactive)
  (let* ((project-dir (project-prompt-project-dir))
         (persp-name (suzu/project-name-function project-dir)))
    (persp-switch persp-name)
    (message project-dir)
    (project-switch-project project-dir)
    ))

(provide 'suzu-project)
