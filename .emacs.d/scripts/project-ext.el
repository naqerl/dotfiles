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

(keymap-set project-prefix-map "e" 'project-ext-keymap)

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
  (message "After project created at %s" project-path)
  (project-remember-projects-under project-path)
  (run-hook-with-args 'project-ext:new-created-hook project-path))

; end-region   -- Create project extensions

; begin-region -- Project perspective extensions
(with-eval-after-load 'perspective
  (require 'perspective)
  (defun project-ext:persp-compilation-buffer-name-function (_)
    "Creates unique compilation buffer name based on the current perspective."
    (concat "*" (project-name (project-current)) "/compile*"))

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
    (message project-dir)
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
    (message "Searching projects in %s" directory)
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
      (message "Total projects found: %s" projects-found)
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
  (message "[project-ext]: Etags mode activated")
  (if project-ext:etags-mode
      (progn
        (add-hook 'after-save-hook 'project-ext:etags--generate 0 t)
        (project-ext:etags--read))
    (remove-hook 'after-save-hook 'project-ext:etags--generate)))

(defun project-ext:etags--read ()
  "Reads TAGS table from the project root."
  (message "Reading etags table")
  (let ((project-tags-table
         (and (project-current)
              (expand-file-name "TAGS"
                                (project-root (project-current))))))
    (when (and project-tags-table (file-exists-p project-tags-table))
      (visit-tags-table project-tags-table t))))

(defun project-ext:etags--generate ()
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
             #'async-shell-command "git ls-files | ctags -ReL -")
          (message
           "Project etags will be generated only for git repository.")))
    (message "Etags will be generated only inside a project.")))


;;

; end-region   -- Etags

; begin-region -- Environment

(defvar project-ext:env-dotenv-file-name ".env"
  "The name of the .env file.")

(defun project-ext:env-load ()
  "Export all environment variables in the closest .env file."
  (let ((env-file (project-ext:env--find-file)))
    (when env-file
      (load-env-vars env-file))))

(defun project-ext:env--find-file ()
  "Searches for the closes .env file."
  (let* ((env-file-directory
          (locate-dominating-file
           "." project-ext:env-dotenv-file-name))
         (file-name
          (concat
           env-file-directory project-ext:env-dotenv-file-name)))
    (when (file-exists-p file-name)
      file-name)))

; end-region   -- Environment

(provide 'project-ext)
;;; project-ext.el ends here
