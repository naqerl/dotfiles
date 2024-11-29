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
   '(alert all-the-icons-completion all-the-icons-dired bufler consult
           corfu csv-mode dashboard dimmer dired-open dockerfile-mode
           drag-stuff eldoc-box elf-mode elfeed-goodies elisp-autofmt
           elysium emojify enwc eshell-git-prompt
           eshell-syntax-highlighting eshell-toggle
           evil-nerd-commenter flycheck-mypy gcmh general git-gutter
           go-mode golden-ratio gptel helpful indent-guide json-mode
           jtsx load-env-vars magit marginalia markdown-mode
           nerd-icons-corfu nushell-mode nushell-ts-mode orderless
           org-auto-tangle org-present pcmpl-args peep-dired
           perspective php-mode plantuml-mode poetry prettier-js
           python-isort rainbow-mode ruff-format rust-mode sqlformat
           sudo-edit tldr toc-org typescript-mode undo-tree vertico
           vterm-toggle web-mode yaml-mode yuck-mode))
 '(warning-suppress-types '((comp))))
