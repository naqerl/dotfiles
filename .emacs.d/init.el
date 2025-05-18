;; Simplify UI
(blink-cursor-mode t)
(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq-default auto-save-default nil)
(setq save-interprogram-paste-before-kill t)
(setq-default scroll-margin 7)
(electric-pair-mode 1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(global-auto-revert-mode t)
(setq help-window-select t)
(setq-default history-length 25)
(savehist-mode 1)
(save-place-mode 1)
(setq use-dialog-box nil)
(winner-mode +1)
(add-to-list 'default-frame-alist '(left-fringe . 0))
(add-to-list 'default-frame-alist '(right-fringe . 0))
(setq-default
 left-margin-width 1
 right-margin-width 0
 async-shell-command-buffer 'new-buffer)
(dolist (mode
         '(prog-mode-hook
           org-mode-hook
           conf-mode-hook
           text-mode))
  (add-hook mode 'display-line-numbers-mode))
(setq-default display-line-numbers-type 'visual)
(setq-default indent-tabs-mode nil)
(electric-indent-mode t)
(setq-default electric-indent-inhibit t)
(setq backward-delete-char-untabify-method 'hungry)
(use-package which-key
  :diminish which-key-mode
  :config (which-key-mode t))
(setq dired-dwim-target t)
(use-package ansi-color
  :config
  (defun user/ansi-colorize-buffer ()
    (let ((buffer-read-only nil))
      (ansi-color-apply-on-region (point-min) (point-max))))
  :hook
  (compilation-filter . user/ansi-colorize-buffer))
(use-package compile
  :custom
  (compilation-max-output-line-length 5000)
  (compilation-scroll-output t)
  :bind
  ("<f8>" . recompile)
  ("<f9>" . project-compile)
  :config
  (add-to-list
   'compilation-error-regexp-alist-alist
   '(biome-lint
     "^\\(.*\\):\\([0-9]+\\):\\([0-9]+\\)\s.*\s━+$" 1 2 3 2 1))
  (add-to-list 'compilation-error-regexp-alist 'biome-lint)
  (add-to-list
   'compilation-error-regexp-alist-alist
   '(tsc
     "^\\(.*\\):\\([0-9]+\\):\\([0-9]+\\)\s-\serror\s.*$" 1 2 3 2 1))
  (add-to-list 'compilation-error-regexp-alist 'tsc))
(use-package eldoc
  :config
  (global-eldoc-mode -1))

;; Fonts
(set-face-attribute 'default nil
                    :font "Iosevka"
                    :height 130
                    :weight 'medium)
(set-face-attribute 'variable-pitch nil
                    :font "Iosevka"
                    :height 130
                    :weight 'medium)
(set-face-attribute 'fixed-pitch nil
                    :font "Iosevka"
                    :height 1.0
                    :weight 'medium)

;; Start server on launch
(use-package server
  :defer 1
  :config
  (setq server-client-instructions nil)
  (unless (server-running-p)
    (server-start)))

;; Add custom files to path
(dolist (file '("scripts"))
  (add-to-list 'load-path (expand-file-name file user-emacs-directory)))

;; Custom built-in binds
(use-package emacs
  :bind
  ("C-," . previous-buffer)
  ("C-." . next-buffer)
  ("C-x C-b" . ibuffer)
  ("C-x k" . kill-current-buffer)
  ("C-x K" . kill-buffer)
  ("C-x /" . comment-or-uncomment-region)
  ("C-c d" . duplicate-line)
  ("C-c r" . replace-regexp)
  ("C-c R" . replace-string))

;; Custom simple binds
(defun user/scroll-half-down ()
  "Scroll down half a window."
  (interactive)
  (scroll-down (floor (/ (window-height) 2))))

(defun user/scroll-half-up ()
  "Scroll up half a window."
  (interactive)
  (scroll-up (floor (/ (window-height) 2))))

(use-package emacs
  :bind
  ("C-v" . user/scroll-half-up)
  ("M-v" . user/scroll-half-down))

;; Eshell
(use-package
  eshell
  :bind
  (:map
   eshell-command-mode-map
   ("C-l" .
    (lambda ()
      (interactive)
      (eshell/clear-scrollback))))
  :config
  (setq
   eshell-buffer-maximum-lines 10000
   eshell-scroll-to-bottom-on-input t
   eshell-history-append t
   eshell-visual-commands '("make" "podman run" "bash" "btop" "ssh" "psql")))

;; Tramp
(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
      (format "%s\\|%s"
              vc-ignore-dir-regexp

              tramp-file-name-regexp))
(setq tramp-verbose 1)

;; Hippie expand
(use-package hippie-exp
  :bind ("M-/" . hippie-expand))

(use-package upcase-abbrev-expand
  :after hippie-exp
  :config
  (add-to-list
   'hippie-expand-try-functions-list 'try-complete-upcase-abbrev))

;; Auth source
(use-package auth-source
  :custom
  (auth-sources '("~/.authinfo.gpg"))
  (auth-source-debug 'trivia)
  (epg-pinentry-mode 'loopback)
  :config
  (auth-source-pass-enable))

;; Custom packages
(load-file (expand-file-name "scripts/my-extensions.el" user-emacs-directory))

(use-package project
  :custom
  (project-mode-line t)
  :bind
  ("C-x p F" . project-root-find-file))

(use-package project-ext
  :after project
  :bind
  ("C-x p e" . project-ext:project-or-default-eshell)
  ("C-x p p" . project-ext:project-switch))

(use-package make-project
  :load-path "scripts"
  :bind ("C-x p c" . make-project-run))

(use-package koi-theme
  :load-path "scripts"
  :config
  (load-theme 'koi t))

(use-package app-launcher)

;; Third party packages
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(require 'package)
(setq package-quickstart t)
(require 'use-package)

(use-package diminish :ensure t)
(use-package sudo-edit :ensure t)
(use-package f :ensure t)

(use-package expand-region
  :ensure t
  :bind
  ("C-;" . er/expand-region))

(use-package eat
  :ensure t
  :diminish eat-eshell-mode
  :hook
  (eshell-mode . eat-eshell-mode)
  (eshell-mode . eat-eshell-visual-command-mode))

(use-package magit
  :ensure t
  :custom
  (magit-status-buffer-switch-function 'switch-to-buffer)
  (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  :bind
  ("C-x g o" . magit)
  ("C-x g c" . magit-commit)
  :hook
  (magit-status-mode . display-line-numbers-mode))

(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :custom
  (git-gutter:modified-sign "~")
  (git-gutter:added-sign "+")
  (git-gutter:deleted-sign "-")
  :bind
  ("M-)" . git-gutter:next-hunk)
  ("M-(" . git-gutter:previous-hunk)
  :hook
  ((org-mode prog-mode) . git-gutter-mode))

(use-package vertico
  :ensure t
  :custom
  (vertico-count 13)
  (vertico-resize nil)
  (vertico-cycle nil)
  :config
  (vertico-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :ensure t
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'left)
  :config
  (marginalia-mode))

(use-package consult
  :ensure t
  :config
  (setq consult-ripgrep-args (concat consult-ripgrep-args " --hidden"))
  :bind
  ("C-x p g" . consult-ripgrep)
  ("C-x b" . consult-buffer)
  ("M-g i" . consult-imenu)
  ("M-g l" . consult-line))

(use-package embark
  :ensure t
  :bind
  ("M-," . embark-act)
  ("M-." . embark-dwim))

(use-package embark-consult
  :ensure t
  :after embark
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
  :config
  (dolist (override '((consult-grep . embark-export)
                      (consult-ripgrep . embark-export)))
    (add-to-list 'embark-default-action-overrides override)))

(use-package golden-ratio
  :ensure t
  :diminish golden-ratio-mode
  :config
  (golden-ratio-mode 1)
  (add-hook 'ediff-startup-hook '(lambda () (golden-ratio-mode -1)) t)
  :custom
  (golden-ratio-auto-scale t)
  (golden-ratio-exclude-buffer-names '("*Occur*" "*xref*" "*Async Shell Command*")))

(use-package dumb-jump
  :ensure t
  :custom
  (dumb-jump-rg-search-args "--pcre2 --max-filesize 80M --no-ignore --hidden")
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package gptel
  :ensure t
  :after f
  :config
  (setq
   gptel-log-level 'info
   gptel-default-mode 'markdown-mode
   gptel-model 'gemini-2.0-flash
   gptel-backend
   (gptel-make-gemini
       "Gemini"
     :key 'gptel-api-key-from-auth-source
     :stream t))
  (let* ((prompts-path (expand-file-name "prompts" user-emacs-directory))
         (prompts (eval (car (read-from-string (f-read prompts-path))))))
    (dolist (prompt prompts)
      (add-to-list 'gptel-directives prompt)))
  :bind ("C-c g" . gptel-menu))

(use-package eradio
  :ensure t
  :defer 1
  :demand
  :custom
  (eradio-player '("mpv" "--no-video" "--no-terminal"))
  (eradio-channels
   (eval (car (read-from-string (f-read (expand-file-name
                                         "eradio-playlist"
                                         user-emacs-directory)))))))

;;; Languages setup:
;; Python
(use-package python
  :custom
  (python-indent-def-block-scale 1)
  :hook (before-save . delete-trailing-whitespace))

(use-package python-tests
  :after python
  :bind (:map python-mode-map ("C-x t r" . python-tests-run)))

;; Emacs lisp
(use-package flymake
  :custom
  (elisp-flymake-byte-compile-load-path load-path))

;; Javascript
(use-package jtsx
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . jtsx-tsx-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . jtsx-jsx-mode)))

;; Do not require config
(use-package yaml-mode :ensure t)
(use-package markdown-mode :ensure t)
(use-package treesit-auto :ensure t)
(use-package load-env-vars :ensure t)
(use-package solidity-mode :ensure t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(diminish dumb-jump eat emacs-mini-frame embark-consult eradio
              expand-region f git-gutter golden-ratio gptel jtsx
              load-env-vars magit marginalia markdown-mode orderless
              solidity-mode sudo-edit treesit-auto vertico yaml-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(completions-annotations ((t (:inherit shadow))))
 '(marginalia-documentation ((t (:foreground "LavenderBlush4" :underline nil)))))
