(org-babel-load-file
 (expand-file-name "config.org" user-emacs-directory))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(compile-eslint counsel eat git-gutter-fringe jsdoc make-project
                    nginx-mode rust-docs))
 '(package-vc-selected-packages
   '((compile-eslint :url "https://github.com/Fuco1/compile-eslint")
     (rust-docs :url "https://github.com/scipunch/rust-docs")))
 '(warning-suppress-types '((comp))))
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
