(org-babel-load-file
 (expand-file-name "config.org" user-emacs-directory))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("3bac409022f5772483b802a9d5b78155bf9f4256cc5dc556426fcd078709218c"
     default))
 '(package-selected-packages '(git-gutter-fringe make-project nginx-mode rust-docs))
 '(package-vc-selected-packages '((rust-docs :url "https://github.com/scipunch/rust-docs")))
 '(warning-suppress-types '((comp))))
(put 'downcase-region 'disabled nil)
