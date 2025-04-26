;; -*- lexical-binding: t; -*-

(defvar eww-ext:search-engines :doc
  "Alist of search engines.")

(defun eww-ext:search ()
  (interactive)
  (let* ((engine-name
          (completing-read "Search engine: " eww-ext:search-engines))
         (search-template
          (cdr (assoc engine-name eww-ext:search-engines)))
         (search-term (read-string "Search for: "))
         (url (format search-template (url-encode-url search-term))))
    (eww url)))

(provide 'eww-ext)
