(defun suzu/flatten-list (list)
  "Flatten a list of lists."
  (cond
    ((null list) nil)
    ((atom list) (list list))
    (t (apply 'append (mapcar #'flatten-list list)))
  ))

(provide 'suzu-extensions)
