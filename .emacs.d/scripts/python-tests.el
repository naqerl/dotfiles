(defun python-tests-run ()
  "Runs currently selected python test."
  (interactive)
  (let* ((test-name (buffer-substring (mark) (point)))
        (test-file-name (buffer-file-name))
        (pytest-args "--capture=no --pdb")
        (pytest-cmd (format "uv run pytest %s -v %s -k %s"
               pytest-args
               test-file-name
               test-name)))
    (pdb pytest-cmd)))

(provide 'python-tests)
