;;; go-doc.el --- Navigate go doc from emacs ;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Tired keeping multiple web pages of Go lang doc for the search?
;;; With this package you can access all of them right from the GNU Emacs
;;; and useful completing helperls like `orderless'
;;; Code:

(defvar go--index-cache nil
  "Contains searched index for each package.")

(defvar go--doc-buffer "*go doc*"
  "Buffer name to show documentation.")

;;;###autoload
(defun go-doc()
  "Search for the Go lang documentation."
  (interactive)
  (unless (project-current)
    (error "It works only inside project"))
  (require 'go-mode)
  (let* ((default-directory (project-root (project-current)))
	 (root-package (completing-read "Package: " (go--list-packages)))
	 (index (go--package-index root-package))
	 (doc-entry (completing-read "Entry: " index))
	 (result (cadr (assoc doc-entry index)))
	 (doc (shell-command-to-string (format "go doc %s" result))))
    (with-current-buffer (get-buffer-create go--doc-buffer)
      (read-only-mode -1)
      (erase-buffer)
      (save-excursion (insert doc))
      (godoc-mode)
      (read-only-mode 1)))
  (display-buffer go--doc-buffer)
  (other-window 1))

(defun go--list-packages ()
  "Lists all packages for the current project."
  (append
   (process-lines "go" "list" "-m" "all")
   (process-lines "go" "list" "std")))

(defun go--package-index (package)
  "Lists one line doc for the all public entitites of the PACKAGE.
Returns (TITLE ENTRY)
ENTRY value can be passed directly to `go doc' command"
  (unless (assoc package go--index-cache)
    (message "Reading documentation...")
    (add-to-list
     'go--index-cache
     (cons package (let ((p (go--truncate-version package)))
		     (apply #'append (mapcar
				      (apply-partially #'go--list-short-doc p)
				      (go--inner-packages p)))))))
  (cdr (assoc package go--index-cache)))

(defun go--list-short-doc (root-package package)
  "Lists one line documentation for the given PACKAGE.
ROOT-PACKAGE is requred for the better title during `compleating-read'"
  (mapcar (lambda (doc)
	    (let ((doc (string-trim-left doc))
		  (entry (go--entry-name doc))
		  (package-name (string-replace root-package "" package)))
	      (list
	       (if (string-empty-p package-name)
		   doc
		 (format "%s at %s" doc package-name))
	       (format "%s.%s" package entry))))
	  (go--short-doc package)))

(defun go--inner-packages (package)
  "Lists all packages of the PACKAGE."
  (seq-filter
   (apply-partially #'s-starts-with-p package)
   (process-lines-ignore-status "go" "list" (format "%s/..." package))))

(defun go--short-doc (for)
  "Gives one line doc for FOR."
  (process-lines "go" "doc" "-short" for))

(defun go--truncate-version (package)
  "Remove version info from the PACKAGE."
  (car (split-string package " ")))

(defun go--entry-name (doc-string)
  "Extract the name from a short go DOC-STRING.
For example, from `func Foo`, extracts `Foo`.
Returns nil if no match."
  (string-match "\\(?:func\\|type\\|const\\|var\\)\s\\(?:(.*)\s\\)?\\([[:word:]]+\\)" doc-string)
  (and (match-string 1 doc-string)))

(provide 'go-doc)
;;; go-doc.el ends here
