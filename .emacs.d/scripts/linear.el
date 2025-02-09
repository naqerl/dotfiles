;;; linear.el --- Linear integratoin for GNU Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;; Helps to create and read linear tasks inside Org mode

;;; Code:
(require 'graphql-mode)
(require 'graphql)
(require 'request)
(require 'cl-lib)

(defvar linear-graphql-url "https://api.linear.app/graphql"
  "Linear API host.")

(defvar linear-api-key
  nil
  "Personal authorization token.")

(defvar linear-home-directory (expand-file-name "~/notes/org/linear")
  "Directory for the linear org files.")

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
  (let ((query
         (graphql-query
          ((projects (nodes id name description state))))))
    (thread-last
     (linear--execute query)
     (alist-get 'data)
     (alist-get 'projects)
     (alist-get 'nodes))))

(defun linear--fetch-project-issues (project-id)
  "Returns issues for PROJECT-ID, optionally filtered by MILESTONE-ID.
Returns a list of issues with their details."
  (let ((query (with-cu)))
    (thread-last
     (linear--execute query `((projectId . ,project-id)))
     (alist-get 'data)
     (alist-get 'issues)
     (alist-get 'nodes))))

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
      (linear--log-info "Creating linear home direcotry at %s"
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
        (linear--log-info
         "Error creating org file for project %s: %s"
         (alist-get 'name project) (error-message-string err))))

     (linear--log-info "Sync completed. Processed %d projects."
                       created-files))))

(defun linear--org-create-project (project)
  "Creates PROJECT org file.
PROJECT is an alist containing the Linear project data."
  (let* ((project-name (alist-get 'name project))
         (project-description (alist-get 'description project))
         (project-directory-name (expand-file-name project-name))
         ct
         (project-issues-directory-name
          (expand-file-name "issues" project-directory-name))
         (project-index-file-name
          (expand-file-name "index.org" project-directory-name)))

    (unless (file-exists-p project-directory-name)
      (make-directory project-directory-name t))

    (unless (file-exists-p project-issues-directory-name)
      (make-directory project-issues-directory-name t))

    ;; Create org file with project details
    (if (file-exists-p project-index-file-name)
        nil
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
  (cl-loop
   for issue across issues do
   (progn
     (linear--log-info "Issue=%s" issue))))

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
