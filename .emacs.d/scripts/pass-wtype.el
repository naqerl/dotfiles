;; -*- lexical-binding: t; -*-
(require 'password-store)

;;;###autoload
(defun user/pass-wtype (entry)
  (interactive (list (password-store--completing-read t)))
  (password-store-get
   entry
   (lambda (password)
     (shell-command (concat "wtype " (shell-quote-argument password))))))

;;;###autoload
(defun emacs-run-pass-wtype ()
  (interactive)
  (with-selected-frame (make-frame '((name . "emacs-run-launcher")
                                     (minibuffer . only)
                                     (auto-raise . t)
                                     (fullscreen . 0)
                                     (width . 70)
                                     (height . 11)))
    (unwind-protect
        (call-interactively 'user/pass-wtype)
      (delete-frame))))

(provide 'pass-wtype)
