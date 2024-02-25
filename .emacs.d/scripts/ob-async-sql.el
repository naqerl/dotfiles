(provide 'ob-async-sql)

(require 'org)

(defun org-babel-async-execute-sql ()
  "Run a SQL block at point asynchrously."
  (interactive)

  (let ((current-file (buffer-file-name))
        (uuid (org-id-uuid))
        (temporary-file-directory "./")
        (tempfile (make-temp-file "sql-"))
	(connect-set (cdr (assoc-string (cdr (assoc :dbconnection (nth 2 (org-babel-get-src-block-info)))) sql-connection-alist t))))

    (org-babel-tangle '(4) tempfile)
    (org-babel-remove-result)
    (save-excursion
      (re-search-forward "#\\+END_SRC")
      (insert (format
               "\n\n#+RESULTS:%s\n: %s"
               (or (org-element-property :name (org-element-context))
                   "")
               uuid)))

    (async-start
     `(lambda ()
        (prog1
            (shell-command-to-string (format "PGPASSWORD=%s psql -h %s -p %s -U %s -d %s -f %s", 
					     (car (cdr (assoc 'sql-password connect-set))),
					     (car (cdr (assoc 'sql-server connect-set))),
					     (car (cdr (assoc 'sql-port connect-set))),
					     (car (cdr (assoc 'sql-user connect-set))),
					     (car (cdr (assoc 'sql-database connect-set))),
					     tempfile))
            (delete-file ,tempfile)))

     `(lambda (result)
        "Code that runs when the async function finishes."
        (save-window-excursion
          (save-excursion
            (save-restriction
              (with-current-buffer (find-file-noselect ,current-file)
                (goto-char (point-min))
                (re-search-forward ,uuid)
                (beginning-of-line)
                (kill-line)
                (insert (mapconcat
                         (lambda (x)
                           (format "| %s |" x))
                         (butlast (s-split "\n" result))
                         "\n"))))))))
))
