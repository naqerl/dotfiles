(require 'project)
(require 'suzu-extensions)

(defvar suzu/compilation-setup-name nil)
(defun suzu/project-compilation-buffer-name-function (maj-mode)
  (concat
   "*"
   (project-name (project-current))
   "/"
   suzu/compilation-setup-name
   "*"))

(setq compilation-buffer-name-function
      'suzu/project-compilation-buffer-name-function)

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
  (let* ((project-dir (project-prompt-project-dir)))
    (my/project-perspective-from-project project-dir)))

(defun my/project-perspective-from-project (project-dir)
  (let* ((persp-name
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

(cl-defstruct compilation-setup name command)
(defvar suzu/compilation-setup-base-path
  (expand-file-name "projects-setup" user-emacs-directory)
  "Directory path for saving project compilation setup")

(defun suzu/project--init-setup-if-needed (project-setup-path)
  (when (not (file-directory-p suzu/compilation-setup-base-path))
    (message "Creating directory %s" suzu/compilation-setup-base-path)
    (make-directory suzu/compilation-setup-base-path))
  (message "Working with file %s" project-setup-path)
  (when (not (file-exists-p project-setup-path))
    (write-region "()" nil project-setup-path)))

(defun suzu/project--read-project-setup (project-setup-path)
  (with-temp-buffer
    (insert-file-contents project-setup-path)
    (cl-assert (eq (point) (point-min)))
    (read (current-buffer))))

(defun suzu/project--update-project-setup
    (project-setup-path project-setup)
  (with-temp-file project-setup-path
    (prin1 project-setup (current-buffer))))

(defun suzu/project--request-setup-name (project-setup)
  (completing-read
   "Choose setup name: "
   (mapcar '(lambda (s) (compilation-setup-name s)) project-setup)))

(defun suzu/project--request-setup-command (project-setup setup-name)
  (let ((prev-command
         (seq-find
          '(lambda (s)
             (string= (compilation-setup-name s) setup-name))
          project-setup)))
    (let ((default-directory (project-root (project-current))))
      (read-shell-command "Input cmd: "
                          (if prev-command
                              (compilation-setup-command prev-command)
                            "")))))

(defun suzu/project-add-compilation-command ()
  "Saves compilation command for the given project"
  (interactive)
  (let ((project-setup-path
         (expand-file-name (project-name (project-current))
                           suzu/compilation-setup-base-path)))
    (suzu/project--init-setup-if-needed project-setup-path)
    (let* ((project-setup
            (suzu/project--read-project-setup project-setup-path))
           (setup-name
            (suzu/project--request-setup-name project-setup))
           (setup-command
            (suzu/project--request-setup-command
             project-setup setup-name)))
      (setq project-setup
            (seq-filter
             '(lambda (s)
                (not (string= (compilation-setup-name s) setup-name)))
             project-setup))
      (add-to-list
       'project-setup
       (make-compilation-setup
        :name setup-name
        :command setup-command))
      (suzu/project--update-project-setup
       project-setup-path project-setup))))

(defun suzu/project-compile ()
  "Execute one of the project compilation commands"
  (interactive)
  (let* ((project-setup-path
          (expand-file-name (project-name (project-current))
                            suzu/compilation-setup-base-path))
         (project-setup
          (suzu/project--read-project-setup project-setup-path))
         (setup-name (suzu/project--request-setup-name project-setup))
         (setup
          (seq-find
           '(lambda (s)
              (string= (compilation-setup-name s) setup-name))
           project-setup)))
    (let ((default-directory (project-root (project-current))))
      (message "Starting compilation at %s" default-directory)
      (setq suzu/compilation-setup-name setup-name)
      (compile (compilation-setup-command setup)))))


(defun my/parse-makefile ()
  "Search for the Makefile in the default-directory and return a table of available targets."  
  (let ((makefile (expand-file-name "Makefile")))
    (if (file-exists-p makefile)
        (with-temp-buffer
          (insert-file-contents makefile)
          (sp/parse-makefile))
      (message "No Makefile found in project root"))))

(defun my/makefile-compile ()
  (interactive)
  (let* ((default-directory (project-root (project-current)))
         (targets (my/parse-makefile))
         (completion-extra-properties
           '(:annotation-function
             (lambda (k)
               (let ((desc
                      (alist-get k minibuffer-completion-table
                                 nil
                                 nil
                                 #'string=)))
                 (format "\t%s" desc)))))
         (target (completing-read "Make target: " targets)))
    (compile (format "make %s" target))))

(provide 'suzu-project)
