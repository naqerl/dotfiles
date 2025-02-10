;;; linear.el --- Linear integratoin for GNU Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;; Helps to create and read linear tasks inside Org mode

;;; Code:
(require 'graphql-mode)
(require 'request)
(require 'cl-lib)

(defvar linear-graphql-url "https://api.linear.app/graphql"
  "Linear API host.")

(defvar linear-api-key
  nil
  "Personal authorization token.")

(defvar linear-home-directory (expand-file-name "~/notes/org/linear")
  "Directory for the linear org files.")

(defvar linear--graphql-find-projects "
  query {
    projects {
      nodes {
        id
        name
        description
        state
      }
    }
  }"
  "Get proects.")

(defvar linear--graphql-find-issues "
  query findIssues($projectId: String!) {
    project(id: $projectId) {
      issues {
        nodes {
          id
          identifier
          title
          state {
            name
          }
          projectMilestone {
            name
          }
        }
      }
    }
  }"
  "Get issues by project id.")

(defmacro linear--execute (query &optional variables)
  "Executes GraphQL linear QUERY.
Accepts optional VARIABLES"
  `(let* ((graphql-extra-headers `((Authorization . ,linear-api-key)))
          (response
           (graphql-post-request
            linear-graphql-url ,query nil ,variables)))
     (when (>= (request-response-status-code response) 400)
       (error-message-string
        (format "[linear-error] Failed to execute query %s with %s"
                ,query response)))
     (request-response-data response)))

; begin-region -- GraphQL

(defun linear--fetch-projects ()
  "Returns a vector of current projects."
  (thread-last
   (linear--execute linear--graphql-find-projects)
   (alist-get 'data)
   (alist-get 'projects)
   (alist-get 'nodes)))

(defun linear--fetch-project-issues (project-id)
  "Returns issues for PROJECT-ID, optionally filtered by MILESTONE-ID.
Returns a list of issues with their details."
  (thread-last
   (linear--execute linear--graphql-find-issues
                    `((projectId . ,project-id)))
   (alist-get 'data)
   (alist-get 'project)
   (alist-get 'issues)
   (alist-get 'nodes)))

; end-region   -- GraphQL

; begin-region -- Org Mode

(defun linear-sync ()
  "Syncs org mode files with linear."
  (interactive)
  (linear--log-info "Starting Linear sync...")

  (let ((projects (linear--fetch-projects))
        (created-files 0)
        (default-directory linear-home-directory))
    (linear--log-info "Fetched %s projects" (length projects))
    ;; Create directory if it doesn't exist
    (unless (file-exists-p linear-home-directory)
      (linear--log-info "Creating linear home directory at %s"
                        linear-home-directory)
      (make-directory linear-home-directory t))

    ;; Process each project
    (cl-loop
     for project across projects do
     (condition-case err
         (progn
           (when (linear--org-create-project project)
             (setq created-files (1+ created-files))
             (linear--log-info "Created project: %s"
                               (alist-get 'name project)))
           (linear--org-create-issues
            (alist-get 'name project)
            (linear--fetch-project-issues (alist-get 'id project))))
       (error
        (linear--log-info "Error during creating project %s: %s"
                          (alist-get 'name project)
                          (error-message-string err))))

     (linear--log-info "Sync completed. Processed %d projects."
                       created-files))))

(defun linear--org-create-project (project)
  "Creates PROJECT org file.
PROJECT is an alist containing the Linear project data."
  (let* ((project-name (alist-get 'name project))
         (project-description (alist-get 'description project))
         (default-directory
          (linear--org-project-name-to-directory project-name))
         (project-index-file-name "index.org"))
    ;; Create project directory if not exists
    (unless (file-exists-p default-directory)
      (make-directory default-directory t))
    ;; Create org file with project details
    (unless (file-exists-p project-index-file-name)
      (with-temp-file project-index-file-name
        (insert (format "#+TITLE: %s\n" project-name))
        (insert (format "#+LINEAR_ID: %s\n" (alist-get 'id project)))
        (insert (format "#+STATE: %s\n\n" (alist-get 'state project)))
        ;; Add description if available
        (when project-description
          (insert "* Description\n")
          (insert project-description "\n\n"))
        project-index-file-name))))

(defun linear--org-create-issues (project-name issues)
  "Creates ISSUES for the PROJECT-NAME."
  (let ((default-directory
         (expand-file-name
          "issues"
          (linear--org-project-name-to-directory project-name))))
    (unless (file-exists-p default-directory)
      (linear--log-info "Creating issues directory %s"
                        default-directory)
      (make-directory default-directory t))
    (cl-loop
     for issue across issues do
     (progn
       (linear--log-info "Issue=%s" issue)
       (linear--org-create-issue issue)))))

(defun linear--org-create-issue (issue)
  "Cretes ISSUE org file."
  (let* ((file-name
          (concat
           (alist-get 'identifier issue)
           " "
           (alist-get 'title issue)
           ".org"))
         (issue-milestone-name
          (thread-last
           issue
           (alist-get 'projectMilestone) (alist-get 'name)))
         (default-directory
          (if issue-milestone-name
              (expand-file-name issue-milestone-name)
            default-directory)))
    (unless (file-exists-p default-directory)
      (linear--log-info "Creating milestone directory %s"
                        default-directory)
      (make-directory default-directory t))
    (unless (file-exists-p file-name)
      (with-temp-file file-name
        (insert (format "#+TITLE: %s\n" (alist-get 'title issue)))
        (insert (format "#+LINEAR_ID: %s\n" (alist-get 'id issue)))
        (insert
         (format "#+STATE: %s\n"
                 (thread-last
                  issue (alist-get 'state) (alist-get 'name))))
        (insert (concat "\n* " (alist-get 'title issue)))))))

(defun linear--org-project-name-to-directory (project-name)
  "Returns absolute path to the project PROJECT-NAME directory."
  (expand-file-name project-name linear-home-directory))

; end-region   -- Org Mode

; begin-region -- Logging

(defun linear--log-info (format-string &rest args)
  "Log a message with FORMAT-STRING and ARGS, prefixed with [linear].
Works like `message' but adds Linear prefix.
Returns the formatted string."
  (let ((msg
         (apply #'format (concat "[linear] " format-string) args)))
    (message "%s" msg)
    msg))

; end-region   -- Logging

(linear-sync)

(provide 'linear)
;;; linear.el ends here
