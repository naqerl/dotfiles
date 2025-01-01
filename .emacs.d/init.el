(org-babel-load-file
 (expand-file-name "config.org" user-emacs-directory))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(package-vc-selected-packages
   '((make-project :url "https://github.com/scipunch/make-project")
     (vterm-toggle :url "https://github.com/scipunch/vterm-toggle")))
 '(warning-suppress-types '((comp))))
(put 'downcase-region 'disabled nil)
