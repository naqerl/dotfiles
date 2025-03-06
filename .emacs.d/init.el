(org-babel-load-file
 (expand-file-name "config.org" user-emacs-directory))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(all-the-icons-completion all-the-icons-dired better-jumper
                              buffer-move compile-eslint
                              consult-yasnippet csv-mode diminish
                              dired-open dockerfile-mode dumb-jump eat
                              elfeed elisp-autofmt elysium
                              eshell-git-prompt free-keys gcmh
                              git-gutter golden-ratio graphql
                              graphql-mode helpful indent-guide
                              load-env-vars magit make-project
                              marginalia markdown-toc orderless
                              org-auto-tangle pcmpl-args peep-dired
                              plantuml-mode popper prettier-js pulsar
                              request rust-docs rust-mode sqlformat
                              sudo-edit toc-org vertico web-mode
                              ws-butler yuck-mode))
 '(package-vc-selected-packages
   '((make-project :url "https://github.com/scipunch/make-project")))
 '(warning-suppress-types '((comp))))
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
