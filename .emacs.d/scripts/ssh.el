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
  (async-shell-command
   (apply #'format "ssh -v -N -L %s:localhost:%s %s" ssh-port-forward)
   (get-buffer-create
    (apply #'format "*ssh-%s:%s-%s*" ssh-port-forward))))

; end-region   -- Port forwarding

(provide 'ssh)
;;; ssh.el ends here
