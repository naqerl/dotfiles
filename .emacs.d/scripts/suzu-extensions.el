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

(provide 'suzu-extensions)
