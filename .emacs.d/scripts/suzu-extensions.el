(defun suzu/flatten-list (list)
  "Flatten a list of lists."
  (cond
   ((null list)
    nil)
   ((atom list)
    (list list))
   (t
    (apply 'append (mapcar #'flatten-list list)))))

(defun suzu/check-files-and-dirs-exist (files-and-dirs directory)
  "Check if any elements in FILES-AND-DIRS exist in DIRECTORY."
  (cl-some
   (lambda (file)
     (let ((fullpath (expand-file-name file directory)))
       (or (file-exists-p fullpath) (file-directory-p fullpath))))
   files))

(defun my/track-new-directories (commands)
  "Remember all directories in `default-directory`, execute COMMANDS, and return new directories created."
  (let ((initial-dirs-set (my/list-directories)))

    (eval commands)

    (let ((new-dirs-set (my/list-directories))
          result)

      (maphash
       (lambda (dir _)
         (message "Checking dir %s" dir)
         (unless (gethash dir initial-dirs-set)
           (push dir result)))
       new-dirs-set)

      result)))

(defun my/list-directories ()
  (let ((initial-dirs
         (seq-filter
          'file-directory-p (directory-files default-directory)))
        (initial-dirs-set (make-hash-table :test 'equal)))

    (mapc
     (lambda (dir) (puthash dir t initial-dirs-set)) initial-dirs)

    initial-dirs-set))

(provide 'suzu-extensions)
