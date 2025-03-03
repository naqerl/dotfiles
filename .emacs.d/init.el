(org-babel-load-file
 (expand-file-name "config.org" user-emacs-directory))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(package-selected-packages
   '(all-the-icons-completion all-the-icons-dired better-jumper
                              buffer-move compile-eslint
                              consult-yasnippet csv-mode dired-open
                              dockerfile-mode eat elf-mode
                              elfeed-goodies elisp-autofmt elysium
                              emojify eshell-git-prompt
                              eshell-syntax-highlighting free-keys
                              gcmh ggtags git-gutter go-mode
                              golden-ratio graphql graphql-mode
                              helpful indent-guide json-mode
                              load-env-vars magit make-project
                              marginalia markdown-toc orderless
                              org-auto-tangle pcmpl-args peep-dired
                              perspective plantuml-mode popper
                              posframe prettier-js pulsar rainbow-mode
                              request ruff-format rust-docs rust-mode
                              screenshot sqlformat sudo-edit toc-org
                              typescript-mode undo-tree vertico
                              visual-regexp-steroids vterm-toggle
                              web-mode ws-butler yaml-mode yuck-mode))
 '(package-vc-selected-packages
   '((compile-eslint :url "https://github.com/Fuco1/compile-eslint")))
 '(screenshot-font-size 11)
 '(warning-suppress-types '((comp))))
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
