;;; package --- Org babel async SQL execution ;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Allows to start an execution of multiple blocks and come back when they will be finished
;;;
;;; Code:
(require 'org)
(require 'async)

(defun org-babel-async-execute-sql (body params)
  "Run a SQL block at point asynchrously.
BODY contains SQL query
PARAMS not used but required by the `org-babel-execute:sql'"
  (interactive)
  (let ((current-file (buffer-file-name))
        (uuid (org-id-uuid))
        (temporary-file-directory "./")
        (tempfile (make-temp-file "sql-"))
        (time (current-time))
        (connect-set
         (cdr
          (assoc-string
           (cdr
            (assoc
             :dbconnection (nth 2 (org-babel-get-src-block-info))))
           sql-connection-alist
           t))))

    (unless connect-set
      (error "Failed to find dbconnections in src block header args"))

    (with-temp-file tempfile
      (insert body))

    (async-start
     `(lambda ()
        (prog1 (replace-regexp-in-string
                "\n$" ""
                (shell-command-to-string
                 (format
                  "PGPASSWORD=%s psql -h %s -p %s -U %s -d %s -f %s"
                  ,(car (cdr (assoc 'sql-password connect-set)))
                  ,(car (cdr (assoc 'sql-server connect-set)))
                  ,(car (cdr (assoc 'sql-port connect-set)))
                  ,(car (cdr (assoc 'sql-user connect-set)))
                  ,(car (cdr (assoc 'sql-database connect-set)))
                  ,
                  tempfile)))
          (delete-file ,tempfile)))

     `(lambda (result)
        "Code that runs when the async function finishes."
        (message "Query %s finished in %fms"
                 ,uuid
                 ,(* 1000 (float-time (time-since time))))
        (save-window-excursion
          (save-excursion
            (save-restriction
              (with-current-buffer (find-file-noselect ,current-file)
                (goto-char (point-min))
                (re-search-forward ,uuid)
                (beginning-of-line)
                (kill-line)
                (kill-line)
                (org-insert-drawer nil "Output")
                (insert result)
                (insert (format-time-string " at <%F %r>"))))))))
    uuid))

(advice-add
 'org-babel-execute:sql
 :override #'org-babel-async-execute-sql)

(provide 'ob-async-sql)
;;; ob-async-sql.el ends here
