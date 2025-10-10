;;; python-tests --- Run pytest right from the buffer -*- lexical-binding: t; -*-

;;; Code:
(cl-defstruct pytest-test "Info about single pytest test." name file)

(defvar python-tests--previous-test nil
  "Contains info about the previous test run.")

(defvar python-tests-pytest-args "--capture=no --pdb"
  "Additional arguments to pass to pytest.")

;;;###autoload
(defun python-tests-run ()
  "Runs currently selected python test."
  (interactive)
  (let* ((test (python-tests--get-test))
         (pytest-cmd
          (format "uv run pytest %s -v %s::%s"
                  python-tests-pytest-args
                  (pytest-test-file test)
                  (pytest-test-name test))))
    (pdb pytest-cmd)))

(defun python-tests--get-test ()
  "Returns a `pytest-test' to run."
  (if (region-active-p)
      (let ((test
             (make-pytest-test
              :name (buffer-substring (mark) (point))
              :file (buffer-file-name))))
        (setq python-tests--previous-test test)
        test)
    (or python-tests--previous-test
        (error
         "Region should be active or previous test run exist"))))

(provide 'python-tests)
;;; python-tests.el ends here
