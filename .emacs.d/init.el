(setq vc-follow-symlinks t)
(org-babel-load-file
 (expand-file-name "config.org" user-emacs-directory))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-hide ((t nil))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(enwc-wired-device "wlan0")
 '(enwc-wireless-device "wlan0")
 '(org-agenda-files nil)
 '(package-selected-packages
   '(alert
     all-the-icons
     all-the-icons-completion
     all-the-icons-dired
     async
     better-jumper
     bufler
     cape
     consult
     corfu
     csv-mode
     dap-mode
     dashboard
     diminish
     dired-open
     docker-compose-mode
     doom-modeline
     drag-stuff
     eldoc-box
     elfeed
     elfeed-goodies
     elisp-autofmt
     emacs-nm
     emojify
     enwc
     eshell-git-prompt
     eshell-syntax-highlighting
     eshell-toggle
     evil-collection
     evil-nerd-commenter
     flycheck-mypy
     gcmh
     general
     git-gutter
     golden-ratio
     helpful
     indent-guide
     jtsx
     magit
     marginalia
     markdown-mode
     mu4e
     multi-vterm
     nerd-icons-corfu
     nushell-mode
     nushell-ts-mode
     orderless
     org-appear
     org-auto-tangle
     org-bullets
     org-roam
     pcmpl-args
     pdf-tools
     peep-dired
     perspective
     poetry
     prettier-js
     python-black
     python-isort
     rainbow-mode
     rust-mode
     spaceline
     sqlformat
     sudo-edit
     surround
     tldr
     toc-org
     tree-sitter-langs
     treesit-auto
     typescript-mode
     undo-tree
     vertico
     visual-fill-column
     vterm
     vterm-toggle
     web-mode
     which-key
     yuck-mode
     zen-mode))
 '(warning-suppress-types '((comp))))
