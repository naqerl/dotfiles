(defun app-launcher:run ()
  "Create and select a frame called emacs-run-launcher which consists only of a minibuffer and has specific dimensions. Run counsel-linux-app on that frame, which is an emacs command that prompts you to select an app and open it in a dmenu like behaviour. Delete the frame after that command has exited"
  (interactive)
  (with-selected-frame (make-frame
                        '((name . "emacs-run-launcher")
                          (buffer-predicate . (lambda (x) nil))
                          (minibuffer . only)
                          (width . 120)
                          (height . 11)))
    (unwind-protect
        (call-interactively 'package-install)
      (progn (delete-frame)
             (kill-buffer)))))

(provide 'app-launcher)
