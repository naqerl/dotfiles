;; TODO: Add bind for select inside of (progn (search-forward "\"") (set-mark-command nil) (search-forward "\"") (backward-char))
;;; UI\UX
(setq make-backup-files nil
      create-lockfiles nil
      auto-save-default nil
      save-interprogram-paste-before-kill t
      scroll-margin 7
      left-margin-width 1
      right-margin-width 0
      async-shell-command-buffer 'new-buffer
      help-window-select t
      history-length 25
      use-dialog-box nil
      dired-dwim-target t
      electric-indent-inhibit t
      backward-delete-char-untabify-method 'hungry
      display-line-numbers-type 'visual
      indent-tabs-mode nil
      custom-file (expand-file-name ".emacs.custom.el" user-emacs-directory)
      dired-kill-when-opening-new-dired-buffer t
      remote-file-name-inhibit-locks t
      tramp-use-scp-direct-remote-copying t
      remote-file-name-inhibit-auto-save-visited t)

(setopt use-short-answers t)

(blink-cursor-mode 1)
(electric-pair-mode 1)
(global-auto-revert-mode 1)
(savehist-mode 1)
(save-place-mode 1)
(winner-mode 1)
(electric-indent-mode 1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(dolist (frame-setting '((left-fringe . 0)
                         (right-fringe . 0)))
  (add-to-list 'default-frame-alist frame-setting)) ;; Remove unwanted UI

;; Builtin packages setup
(use-package which-key
  :disabled
  :diminish which-key-mode
  :config (which-key-mode t))

(use-package ansi-color ;; Properly handle colors in compilation buffers
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
  (compilation-buffer-name-function (lambda (_) (concat "*" compile-command "*")))
  :bind
  ("<f8>" . recompile)
  ("<f9>" . project-compile)
  :config
  (dolist (regex '((biome-lint "^\\(.*\\):\\([0-9]+\\):\\([0-9]+\\)\s.*\s━+$" 1 2 3 2 1)
                   (tsc "^\\(.*\\):\\([0-9]+\\):\\([0-9]+\\)\s-\serror\s.*$" 1 2 3 2 1)))
    (add-to-list 'compilation-error-regexp-alist-alist regex)
    (add-to-list 'compilation-error-regexp-alist-alist (car regex))))

(use-package eldoc ;; There is no place for the annoying documentation
  :config
  (global-eldoc-mode -1))

(use-package ls-lisp ;; Sort directories first in dired
  :custom
  (ls-lisp-dirs-first t)
  (ls-lisp-use-insert-directory-program nil))

;; Custom built-in binds
(use-package emacs
  :bind
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

(defun user/smart-kill-back()
  "Kill word back if region is not selected else kill region."
  (interactive)
  (call-interactively
   (if (region-active-p)
       'kill-region
       'backward-kill-word)))

(use-package emacs
  :bind
  ("C-v" . user/scroll-half-up)
  ("M-v" . user/scroll-half-down)
  ("C-w" . user/smart-kill-back))

(use-package eshell
  :bind
  (:map eshell-command-mode-map
        ("C-l" . (lambda () (interactive) (eshell/clear-scrollback))))
  :config
  (defun user/eshell-copy-last-output ()
    (interactive)
    (copy-region-as-kill (eshell-beginning-of-output) (eshell-end-of-output))
    (message "Output of %s was copied" eshell-last-command-name))
  (setq
   eshell-buffer-maximum-lines 10000
   eshell-scroll-to-bottom-on-input t
   eshell-history-append t
   eshell-visual-commands '("make" "bash" "btop" "ssh" "psql")
   eshell-visual-subcommands '(("podman" "run")))
  (add-to-list 'savehist-additional-variables '(eshell-history . 255))
  (add-hook
   'eshell-mode-hook
   '(lambda ()
      (bind-key "C-c C-o" #'user/eshell-copy-last-output 'eshell-mode-map))))

;; SSH shortcut
(defun ssh()
  "Completing read ssh server and connect to it."
  (interactive)
  (let* ((default-directory (read-file-name "SSH host: " "/ssh:"))
	(eat-buffer-name (concat "*" default-directory "*")))
    (eat)))

;; Dired
(advice-add 'dired-delete-file :before
            (lambda (file &rest rest)
              (when-let ((buf (get-file-buffer file)))
                (kill-buffer buf))))

(use-package hippie-exp ;; Completion
  :bind ("M-/" . hippie-expand))

(use-package upcase-abbrev-expand
  :after hippie-exp
  :load-path "scripts"
  :config
  (add-to-list
   'hippie-expand-try-functions-list 'try-complete-upcase-abbrev))

(use-package my-extensions :load-path "scripts")

(use-package project
  :custom
  (project-mode-line t)
  :config
  ;; During the work many unrelated buffers to the current project files
  ;; are used, which lead to using of a wrong project or multiple
  ;; "choose project" prompts. Given hack sets a global project
  ;; and uses it until project switched intentionally
  (defvar user/global-project nil "Use single proect per frame")
  (defun user/set-global-project (DIR)
      (setq user/global-project DIR))
  (defun user/get-global-project (orig-fun &optional MAYBE-PROMPT DIRECTORY)
    (apply orig-fun `(MAYBE-PROMPT ,(if DIRECTORY DIRECTORY user/global-project))))
  (advice-add 'project-switch-project :before #'user/set-global-project)
  (advice-add 'project-current :around #'user/get-global-project))

(use-package project-ext
  :after project
  :load-path "scripts"
  :bind
  ("C-x p e" . project-ext:project-or-default-eshell)
  ("C-x p j" . project-ext:project-switch)
  ("C-x p F" . project-ext:root-find-file))

(use-package make-project
  :load-path "scripts"
  :bind ("C-x p c" . make-project-run)
  :custom (compilation-buffer-name-function
	  (lambda (_) (if make-project-compilation-buffer-name
			  make-project-compilation-buffer-name
			(concat "*" (downcase name-of-mode) "*")))))

(use-package org
  :custom
  (org-edit-src-content-indentation 0))

;; Third party packages
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(require 'package)
(require 'use-package)

(use-package diminish :ensure t)
(use-package sudo-edit :ensure t)
(use-package f :ensure t)

(use-package expand-region
  :defer 1
  :ensure t
  :bind
  ("M-:" . er/expand-region))

(use-package eat
  :ensure t
  :diminish eat-eshell-mode
  :hook
  (eshell-mode . eat-eshell-mode)
  (eshell-mode . eat-eshell-visual-command-mode))

(use-package magit
  :defer 1
  :ensure t
  :custom
  (magit-status-buffer-switch-function 'switch-to-buffer)
  (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  :bind
  ("C-x g" . magit))

(use-package git-gutter
  :defer 1
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
  (vertico-count 7)
  :config
  (vertico-mode t)
  (vertico-multiform-mode t))

(use-package embark
  :ensure t
  :bind
  (:map minibuffer-default ("M-," . embark-export)))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package golden-ratio
  :defer 1
  :ensure t
  :diminish golden-ratio-mode
  :config
  (golden-ratio-mode 1)
  (add-hook 'ediff-startup-hook '(lambda () (golden-ratio-mode -1)) t)
  :custom
  (golden-ratio-auto-scale t)
  (golden-ratio-exclude-buffer-names '("*Occur*" "*xref*" "*Async Shell Command*")))

(use-package dumb-jump
  :defer 1
  :ensure t
  :custom
  (dumb-jump-rg-search-args "--pcre2 --max-filesize 80M --no-ignore --hidden")
  (dumb-jump-force-searcher 'rg)
  (dumb-jump-functions-only t)
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package change-inner
  :ensure t
  :bind
  ("M-i" . change-inner)
  ("M-o" . change-outer))

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
  :disabled
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . jtsx-tsx-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . jtsx-jsx-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . jtsx-typescript-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . jtsx-typescript-mode)))

(use-package web-mode
  :ensure t
  :disabled
  :config
  (setq web-mode-engines-alist
	'(("go"    . "\\.html\\'")))
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode)))

;; Go lang
(use-package go-mode
  :ensure t
  :config
  (defun user/go-mode-hook ()
    (setq tab-width 8
	  standard-indent 8
	  indent-tabs-mode nil))
  (defalias 'user/go-insert-err-check
    (kmacro "C-e RET i f SPC e r r SPC ! = SPC n i l SPC { RET r e t u r n SPC e r r"))
  :hook
  (go-mode . user/go-mode-hook)
  :bind (:map go-mode-map
	      ("C-c C-e" . user/go-insert-err-check)))

(use-package go-doc
  :after go-mode
  :load-path "script"
  :bind (:map go-mode-map
              ("C-c s" . go-doc)))

;; Snippets
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :custom
  (yas-snippet-dirs `(,(expand-file-name
                        "snippets"
                        user-emacs-directory)))
  :config
  (yas-global-mode 1))

;; Do not require config
(use-package yaml-mode :ensure t)
(use-package markdown-mode :ensure t)
(use-package solidity-mode :ensure t)
(use-package treesit-auto :ensure t)
(put 'dired-find-alternate-file 'disabled nil)

(use-package undo-tree
  :defer 1
  :ensure t
  :custom
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

(use-package clipetty
  :disabled
  :ensure t
  :bind ("M-w" . clipetty-kill-ring-save))

(add-hook 'after-init-hook
          (lambda ()
            (set-face-attribute 'default nil :font "Iosevka Term Nerd Font-20")
            (set-face-attribute 'mode-line nil :font "Iosevka Term Nerd Font-14")))

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs started in %s" (emacs-init-time))))

(message "Config loaded")
