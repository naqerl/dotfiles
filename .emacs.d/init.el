(org-babel-load-file
 (expand-file-name "config.org" user-emacs-directory))
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1"
     "b49f66a2e1724db880692485a5d5bcb9baf28ed2a3a05c7a799fa091f24321da"
     "833ddce3314a4e28411edf3c6efde468f6f2616fc31e17a62587d6a9255f4633"
     "7fea145741b3ca719ae45e6533ad1f49b2a43bf199d9afaee5b6135fd9e6f9b8"
     default))
 '(package-selected-packages
   '(all-the-icons-completion all-the-icons-dired better-jumper
                              buffer-move compile-eslint csv-mode
                              diminish dired-open dockerfile-mode
                              dumb-jump eat elfeed elisp-autofmt
                              elysium flimenu free-keys gcmh
                              git-gutter golden-ratio graphql
                              graphql-mode helpful jinx load-env-vars
                              magit make-project marginalia
                              markdown-toc orderless org-auto-tangle
                              org-tempo peep-dired plantuml-mode
                              prettier-js pulsar request rust-docs
                              rust-mode solarized-theme sqlformat
                              sudo-edit toc-org vertico web-mode
                              yasnippet yuck-mode))
 '(package-vc-selected-packages
   '((make-project :url "https://github.com/scipunch/make-project")))
 '(safe-local-variable-directories '("/home/user/code/work/aishift/project-research/"))
 '(safe-local-variable-values
   '((eval progn (add-to-list 'gptel-directives `
                              (aishift-project-analysist \

                               ,(with-temp-buffer
                                  (insert-file-contents
                                   (expand-file-name
                                    "aishift-project-analysist-prompt.org"
                                    (project-root
                                     (project-current))))
                                  (buffer-string)))))))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(line-number ((((class color) (min-colors 89)) (:weight regular :underline nil :foreground "#586e75" :background "#002b36"))))
 '(line-number-current-line ((t (:inherit line-number :background "#002b36" :foreground "#839496" :weight bold))))
 '(org-block-begin-line ((t (:inherit org-meta-line :underline nil))))
 '(org-block-end-line ((t (:inherit org-meta-line :overline nil)))))
