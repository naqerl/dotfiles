(setq vc-follow-symlinks t)
(org-babel-load-file
 (expand-file-name "config.org" user-emacs-directory))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(my-modeline-background ((t (:inherit bold :background "#5f509f" :foreground "white"))))
 '(org-hide ((t nil))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(custom-safe-themes
   '("2f9cff368c07d280a7a766e9f04a0053a17bb74f775504dc49421d1fda2a0797"
     "f700bc979515153bef7a52ca46a62c0aa519950cc06d539df4f3d38828944a2c"
     "6d55be90dc8c7b3027b0ea5bc4cc50c6352e2e4f03823eee58ac23ece8237139"
     "45333f79e4a7fdeff9924d5b6658f84fb468ef38f749455e5b58ba4154782007"
     "1f292969fc19ba45fbc6542ed54e58ab5ad3dbe41b70d8cb2d1f85c22d07e518"
     "b8bd60a23b9e2f08b0c437231ee84f2dacc70fdc4d5a0fb87229bb9926273fdd"
     "e8bd9bbf6506afca133125b0be48b1f033b1c8647c628652ab7a2fe065c10ef0"
     "9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1"
     "bb5b5cec7ef86edbb6486c46ac7dd47694040951f5fe2cf7cbff537eca5dcc87"
     "013728cb445c73763d13e39c0e3fd52c06eefe3fbd173a766bfd29c6d040f100"
     "df6dfd55673f40364b1970440f0b0cb8ba7149282cf415b81aaad2d98b0f0290"
     "93011fe35859772a6766df8a4be817add8bfe105246173206478a0706f88b33d"
     "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8"
     "56044c5a9cc45b6ec45c0eb28df100d3f0a576f18eef33ff8ff5d32bac2d9700"
     "d481904809c509641a1a1f1b1eb80b94c58c210145effc2631c1a7f2e4a2fdf4"
     "34cf3305b35e3a8132a0b1bdf2c67623bc2cb05b125f8d7d26bd51fd16d547ec"
     "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e"
     default))
 '(enwc-wired-device "wlan0")
 '(enwc-wireless-device "wlan0")
 '(org-agenda-files nil)
 '(package-selected-packages
   '(alert all-the-icons-completion all-the-icons-dired base16-theme
           cargo consult corfu csv-mode dashboard dired-open
           dockerfile-mode doom-themes eldoc-box elf-mode
           elfeed-goodies elisp-autofmt elysium emojify enwc
           eshell-git-prompt eshell-syntax-highlighting eshell-toggle
           evil-nerd-commenter flatland-theme flycheck-pos-tip gcmh
           general git-gutter go-mode golden-ratio grayscale-theme
           helpful indent-guide json-mode jtsx kuronami-theme
           load-env-vars magit make-project marginalia markdown-toc
           nerd-icons-corfu nginx-mode nushell-ts-mode orderless
           org-auto-tangle org-present pcmpl-args peep-dired
           perspective pet php-mode plantuml-mode poetry prettier-js
           pulsar rainbow-mode ruff-format rust-mode sqlformat
           sublime-themes sudo-edit surround tao-theme tldr toc-org
           typescript-mode undo-tree vertico visual-regexp-steroids
           vterm-toggle web-mode with-editor yaml-mode yuck-mode))
 '(package-vc-selected-packages
   '((vterm-toggle :url "https://github.com/scipunch/vterm-toggle")
     (make-project :url "https://github.com/scipunch/make-project"
                   :branch "main")))
 '(warning-suppress-types '((comp))))
(put 'downcase-region 'disabled nil)
