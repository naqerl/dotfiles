;;; ssh --- SSH utils ;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Small utils to work with SSH

;;; Code:

(require 'cl-lib)

; begin-region -- Domain

(defvar ssh-port-forward nil
  "List of (PORT-FORWARD-TO PORT-FORWARD-FROM SSH-HOST).")

; end-region   -- Domain

; begin-region -- Port forwarding

(defun ssh-port-forward ()
  "Creates port forward based on variable `ssh-port-forward'."
  (interactive)
  (unless (length= ssh-port-forward 3)
    (error "[ssh]: Wrong variable `ssh-port-forward' format"))
  (let ((output-buffer-name
         (apply #'format "*ssh-%s:%s-%s*" ssh-port-forward)))
    (if (get-buffer output-buffer-name)
        (warn
         "[ssh]: Buffer %s already exists, a new process will not be created"
         output-buffer-name)
      (progn
        (get-buffer-create output-buffer-name)
        (async-shell-command (apply #'format
                                    "ssh -v -N -L %s:localhost:%s %s"
                                    ssh-port-forward)
                             output-buffer-name)
        (message "Starting port forwarding in %s"
                 output-buffer-name)))))

; end-region   -- Port forwarding

; begin-region -- UI Setup

(add-to-list
 'display-buffer-alist '("\\*ssh-.*\\*" (display-buffer-no-window))
 t (lambda (lhs rhs) (string= (car lhs) (car rhs))))

; end-region   -- UI Setup

(provide 'ssh)
;;; ssh.el ends here
