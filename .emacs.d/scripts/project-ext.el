;;;; project-ext --- Custom extensions for the project ;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Additional extension for the project

(require 'my-extensions)
(require 'project)

; begin-region -- Keymap

(defvar-keymap project-ext-keymap
  :doc "Project ext keymap."
  :prefix
  'project-ext-keymap
  "s"
  #'project-ext:search-comment)

(keymap-set project-prefix-map "a" 'project-ext-keymap)

; end-region   -- Keymap

; begin-region -- Create project extensions

(defvar project-ext:new-created-hook nil
  "Hook that runs after project creation.
PROJECT-PATH provided as argument")

(defvar project-ext:new--git-clone-from nil
  "Source to clone from.")

(defvar project-ext:new--name nil
  "Name for the new project.")

(defconst project-git-base-path (expand-file-name "~/code/")
  "Base path to clone to.")

;;; Code:
(defun project-ext:new-git-clone ()
  "Clones project to the given directory."
  (interactive)
  (let* ((default-directory
          (read-directory-name "Base directory: "
                               project-git-base-path))
         (project-ext:new--git-clone-from
          (read-string "Clone from: "))
         (project-dir-name
          (car
           (my/track-new-directories
            '(shell-command
              (format "git clone %s"
                      project-ext:new--git-clone-from)))))
         (project-path
          (expand-file-name (format "%s/" project-dir-name))))
    (project-ext:new--after-created project-path)))

(defun project-ext:new-custom ()
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
    (project-ext:new--after-created default-directory)))

(defun project-ext:new--after-created (project-path)
  "Call after project created and pass PROJECT-PATH."
  (project-ext:info "After project created at %s" project-path)
  (project-remember-projects-under project-path)
  (run-hook-with-args 'project-ext:new-created-hook project-path))

; end-region   -- Create project extensions

; begin-region -- Project perspective extensions
(with-eval-after-load 'perspective
  (require 'perspective)
  (defun project-ext:persp-compilation-buffer-name-function (_)
    "Creates unique compilation buffer name based on the current perspective."
    (concat "*" (persp-current-name) "/compile*"))

  (defun project-ext:persp-switch (&optional project-dir)
    "Opens project in a new perspective.
If PROJECT-DIR not specified then prompts for it"
    (interactive)
    (let ((project-dir (or project-dir (project-prompt-project-dir))))
      (project-ext:persp--switch project-dir)))

  (defun project-ext:persp--switch (project-dir)
    "Creates new perspective for the given PROJECT-DIR."
    (persp-switch
     (project-ext:persp--get-last-two-elements project-dir))
    (project-ext:info project-dir)
    (setq-local project-current-directory-override project-dir)
    (project-find-file))

  (defun project-ext:persp--get-last-two-elements (dir)
    "Get the last two elements of a DIR."
    (let* ((dir-components (split-string dir "\/" t))
           (last-two (last dir-components 2))
           (result
            (if (string-match-p "\\(~\\|suzu\\).*" (car last-two))
                (last last-two 1)
              last-two)))
      (mapconcat 'identity result "/")))

  (defun project-ext:persp-discover (directory &optional depth)
    "Recursively searches projects under given DIRECTORY.
Default DEPTH is 6
Returns number of total found projects"
    (interactive (list (read-directory-name "Base search path: ")))
    (or depth (setq depth 6))
    (when (not (file-directory-p directory))
      (error "Base path should be a directory"))
    (project-ext:info "Searching projects in %s" directory)
    (let ((projects-found
           (project-remember-projects-under directory)))
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
                         (project-ext:persp-discover
                          dir (1- depth))))))))
      (project-ext:info "Total projects found: %s" projects-found)
      projects-found)))
; end-region   -- Project perspective extensions

; begin-region -- Project search extensions

(defvar project-ext:search-comment-list '("todo" "fixme" "xxx"))

;; TODO: Process comments with mentions e.g. 'TODO(scipunch): ...'
(defun project-ext:search-comment (&optional what)
  "Searches for the WHAT comments.
WHAT - element from `project-ext:search--comment-regexp' or string any"
  (interactive)
  (let ((what
         (or what
             (completing-read
              "What: "
              (cl-list* "any" project-ext:search-comment-list)))))
    (project-find-regexp
     (if (string= what "any")
         (string-join (mapcar
                       (lambda (el)
                         (format "\\(%s:\\)" (upcase el)))
                       project-ext:search-comment-list)
                      "\\|")
       (format "%s:" (upcase what))))))

; end-region   -- Project search extensions

; begin-region -- Etags

(define-minor-mode project-ext:etags-mode
  "Autoloads TAGS file from the current project root.
Updates project's TAGS file on every save."
  :init-value
  nil
  (project-ext:info "Etags mode activated")
  (if project-ext:etags-mode
      (progn
        (add-hook 'after-save-hook 'project-ext:etags-generate 100 t)
        (project-ext:etags-read))
    (remove-hook 'after-save-hook 'project-ext:etags-generate t)))

(defun project-ext:etags-read ()
  "Reads TAGS table from the project root."
  (interactive)
  (project-ext:info "Reading etags table")
  (let ((project-tags-table
         (and (project-current)
              (expand-file-name "TAGS"
                                (project-root (project-current))))))
    (when (and project-tags-table (file-exists-p project-tags-table))
      (visit-tags-table project-tags-table t))))

(defun project-ext:etags-generate ()
  "Generates TAGS file for the project root from git files."
  (interactive)
  (if (project-current)
      (let ((default-directory (project-root (project-current)))
            (display-buffer-alist
             '(("*Async Shell Command*"
                display-buffer-no-window
                (nil)))))
        (if (file-exists-p ".git")
            (my/inhibit-sentinel-messages
             #'async-shell-command
             "/usr/local/bin/ctags -Re --exclude='*.json' --exclude='.venv' .")
          (project-ext:info
           "Project etags will be generated only for git repository.")))
    (project-ext:info
     "Etags will be generated only inside a project.")))

; end-region   -- Etags

; begin-region -- Environment

(with-eval-after-load 'load-env-vars
  (require 'load-env-vars)
  (defvar project-ext:dotenv-file-name "^.env$"
    "The name of the .env file.")

  (defun project-ext:dotenv-load (&optional max-depth)
    "Export all environment variables in the closest .env file,
searching up to MAX-DEPTH directories."
    (interactive "p")
    (when (project-current)
      (let ((default-directory (project-root (project-current))))
        (dolist (env-file (project-ext:dotenv--find-files))
          (load-env-vars env-file)
          (project-ext:info "Loaded environment from %s" env-file)))))

  (defun project-ext:dotenv--find-files ()
    "Recursively searches for .env files"
    (directory-files-recursively
     default-directory project-ext:dotenv-file-name))

  (defun project-ext:dotenv--load-advice (&rest rest)
    "Advice function to load dotenv.
REST ommited."
    (when (project-current)
      (let ((default-directory (project-root (project-current))))
        (project-ext:dotenv-load)))))

; end-region   -- Environment

; begin-region -- Timer

(defun project-ext:start-timer ()
  "Starts timer for the given project."
  (interactive)
  ;; TODO: Implement
  ;; Check if inside project
  ;; Get or create entry for the current project
  ;; Get or create entry fro the todays date
  ;; Start timer
  ;; Increase time after timer ends
  )

; end-region   -- Timer

; begin-region -- Logging

(defun project-ext:info (format-string &rest args)
  "Debug message FORMAT-STRING with ARGS."
  (apply #'message
         (format "[project-ext] INFO: %s" format-string)
         args))

; end-region   -- Logging

(provide 'project-ext)
;;; project-ext.el ends here
