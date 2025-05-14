;; -*- lexical-binding: t; -*-
(defun try-complete-upcase-abbrev (old)
  "Try to complete text using ripgrep.
The argument OLD has to be nil the first call of this function, and t
for subsequent calls (for further possible completions of the same
string).  It returns t if a new completion is found, nil otherwise."
  (if (not (project-current))
      nil
    (progn
      (when (not old)
        (he-init-string (he-file-name-beg) (point))
        (let
            ((default-directory (project-root (project-current)))
             (completions
              (mapcar
               's-trim
               (s-split
                "\n"
                (shell-command-to-string
                 (concat
                  "rg"
                  " --color never --no-heading --no-filename --only-matching"
                  " "
                  (shell-quote-argument
                   (concat
                    "\\s"
                    (string-join (s-split "" he-search-string t)
                                 "[a-z0-9]+")
                    "([a-zA-Z0-9]*|[(:_\s]?)"))
                  " | sort -u"))
                t))))
          (message "Completions %s" completions)
          (setq he-expand-list completions)))

      (while (and he-expand-list
                  (he-string-member
                   (car he-expand-list) he-tried-table))
        (setq he-expand-list (cdr he-expand-list)))

      (if (null he-expand-list)
          (progn
            (if old
                (he-reset-string))
            ())
        (let ((completion (car he-expand-list)))
          (he-substitute-string completion)
          (setq he-tried-table (cons completion he-tried-table))
          (setq he-expand-list (cdr he-expand-list))
          t)))))

(provide 'upcase-abbrev-expand)
