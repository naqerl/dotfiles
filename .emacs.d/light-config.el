;; -*- lexical-binding: t; -*-
(require 'treesit)
(add-to-list 'exec-path (expand-file-name "~/.local/bin"))
(setenv "PATH" (concat (mapconcat #'identity exec-path path-separator) (getenv "PATH")))

(setq native-comp-speed 2) ;; maximum native Elisp speed!
(native-compile-async "~/.emacs.d" 'recursively)
(custom-set-variables '(warning-suppress-types '((comp))))

(require 'package-manager)

(use-package gcmh
  :diminish gcmh-mode
  :init
  (gcmh-mode 1))

(setq-default debug-on-error t)

(setq use-package-compute-statistics nil)

(use-package diminish)

(use-package all-the-icons)
(use-package all-the-icons-dired
  :diminish all-the-icons-dired-mode
  :after all-the-icons
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

(use-package dumb-jump
  :custom
  (dumb-jump-rg-search-args "--pcre2 --max-filesize 80M --no-ignore --hidden")
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package eldoc
  :diminish eldoc-mode)

(setq treesit-font-lock-level 4)
(defun my/treesit-install-grammars ()
  "Install Tree-sitter grammars if they are absent."
  (interactive)
  (require 'treesit)
  (dolist (grammar
           '((css
              .
              ("https://github.com/tree-sitter/tree-sitter-css"
               "v0.20.0"))
             (bash "https://github.com/tree-sitter/tree-sitter-bash")
             (html
              .
              ("https://github.com/tree-sitter/tree-sitter-html"
               "v0.20.1"))
             (javascript
              .
              ("https://github.com/tree-sitter/tree-sitter-javascript"
               "v0.21.2"
               "src"))
             (json
              .
              ("https://github.com/tree-sitter/tree-sitter-json"
               "v0.20.2"))
             (python
              .
              ("https://github.com/tree-sitter/tree-sitter-python"
               "v0.20.4"))
             (go
              "https://github.com/tree-sitter/tree-sitter-go"
              "v0.20.0")
             (markdown
              "https://github.com/ikatyang/tree-sitter-markdown")
             (make "https://github.com/alemuller/tree-sitter-make")
             (elisp "https://github.com/Wilfred/tree-sitter-elisp")
             (cmake "https://github.com/uyha/tree-sitter-cmake")
             (c "https://github.com/tree-sitter/tree-sitter-c")
             (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
             (toml "https://github.com/tree-sitter/tree-sitter-toml")
             (tsx
              .
              ("https://github.com/tree-sitter/tree-sitter-typescript"
               "v0.23.2"
               "tsx/src"))
             (typescript
              .
              ("https://github.com/tree-sitter/tree-sitter-typescript"
               "v0.23.2"
               "typescript/src"))
             (yaml
              .
              ("https://github.com/ikatyang/tree-sitter-yaml"
               "v0.5.0"))
             (prisma
              "https://github.com/victorhqc/tree-sitter-prisma")))
    (setq treesit-language-source-alist '())
    (add-to-list 'treesit-language-source-alist grammar)
    (unless (treesit-language-available-p (car grammar))
      (treesit-install-language-grammar (car grammar)))))
(my/treesit-install-grammars)

(use-package emacs
  :bind
  ("C-x /" . comment-or-uncomment-region))

(use-package ssh
  :load-path (lambda () (expand-file-name "scripts/ssh.el" user-emacs-directory)))

(defun my/occur-regions ()
  (interactive)
  (occur (format "^%s begin-region -- .*$" comment-start)))

(use-package
 emacs
 :bind ("M-s r" . my/occur-regions))

(use-package
 emacs
 :bind ("C-c r" . replace-regexp) ("C-c R" . replace-string))

(use-package rust-mode)

(use-package
 python
 :config (setq-default python-indent-def-block-scale 1)
 :hook (before-save-hook . delete-trailing-whitespace))

(use-package
 python-tests
 :load-path
 (lambda ()
   (expand-file-name "scripts/python-tests.el" user-emacs-directory))
 :after python
 :bind (:map python-mode-map ("C-x t r" . python-tests-run)))

(use-package yuck-mode)

(use-package async)
(use-package ob-async-sql
  :load-path (lambda () (expand-file-name "scripts/ob-async-sql.el" user-emacs-directory))
  :after async)

(use-package markdown-mode)

(use-package markdown-toc
  :after markdown-mode)

(use-package csv-mode)

(setq-default js-indent-level 2)
(setq-default web-mode-code-indent-offset 2)

(use-package jtsx
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . jtsx-tsx-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . jtsx-jsx-mode)))

(defun my/web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-enable-auto-pairing t))

(use-package
 web-mode
 :mode (("\\.html?\\'" . web-mode))
 :hook (web-mode-hook . my/web-mode-hook))

(add-to-list
 'compilation-error-regexp-alist-alist
 '(biome-lint
   "^\\(.*\\):\\([0-9]+\\):\\([0-9]+\\)\s.*\s━+$" 1 2 3 2 1))
(add-to-list 'compilation-error-regexp-alist 'biome-lint)

(add-to-list
 'compilation-error-regexp-alist-alist
 '(tsc
   "^\\(.*\\):\\([0-9]+\\):\\([0-9]+\\)\s-\serror\s.*$" 1 2 3 2 1))
(add-to-list 'compilation-error-regexp-alist 'tsc)

(defun my/eval-buffer-and-print ()
  (interactive)
  (eval-buffer)
  (message "Buffer evaluated"))

(defun my/emacs-lisp-mode-hook ()
  (flymake-mode)
  (keymap-set emacs-lisp-mode-map "C-c C-f" 'elisp-autofmt-buffer)
  (keymap-set emacs-lisp-mode-map "C-x C-b" 'my/eval-buffer-and-print))

(use-package
 elisp-autofmt
 :hook (emacs-lisp-mode-hook . my/emacs-lisp-mode-hook))

(with-eval-after-load 'flymake
  (setq elisp-flymake-byte-compile-load-path load-path))

(use-package tex-mode)

(use-package css-mode)

(use-package dockerfile-mode)

(use-package
 plantuml-mode
 :custom
 (org-plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar")
 (plantuml-default-exec-mode 'jar)
 (plantuml-jar-path org-plantuml-jar-path)
 (plantuml-indent-level 2)
 (plantuml-output-type "png")
 (plantuml-jar-args
  `("-charset"
   "UTF-8"
   "-config"
   ,(expand-file-name "plantuml.cfg" user-emacs-directory)))
 (org-plantuml-args
  `("-headless"
   "-config"
   ,(expand-file-name "plantuml.cfg" user-emacs-directory)))
 :hook (plantuml-mode-hook . display-line-numbers-mode))

(use-package
 solidity-mode
 :config
 (defun solidity-at-vsemi-p (&optional pos)
   (let ((rpos (or pos (point))))
     (save-excursion
       (goto-char rpos)
       (ignore-errors
         ;; Try to jump back to the word "struct", as if we're at the end of a
         ;; syntactically-correct struct. Struct body, struct name, the keyword "struct".
         (forward-sexp -3)
         (looking-at-p "\\bstruct\\b")))))
 (add-hook
  'solidity-mode-hook
  (lambda () (setq-local c-at-vsemi-p-fn 'solidity-at-vsemi-p))))

(use-package yaml-mode)

(use-package
 ansi-color
 :config
 (defun my/ansi-colorize-buffer ()
   (let ((buffer-read-only nil))
     (ansi-color-apply-on-region (point-min) (point-max))))
 :hook (compilation-filter-hook . my/ansi-colorize-buffer))

(defvar my/global-compilation-buffer-names-list nil
  "List of names of each compilation buffer")

(defun my/next-error ()
  "Navigates to the next xref or flymake."
  (interactive)
  (if (seq-some
       #'my/window-with-name-visible-p
       (append
        '("*xref*" "*Occur*")
        my/global-compilation-buffer-names-list))
      (next-error)
    (flymake-goto-next-error)))

(defun my/previous-error ()
  "Navigates to the previous xref or flymake."
  (interactive)
  (if (seq-some
       #'my/window-with-name-visible-p
       (append
        '("*xref*" "*Occur*")
        my/global-compilation-buffer-names-list))
      (previous-error)
    (flymake-goto-prev-error)))

(defun my/compilation-hook (process)
  (unless (member
           (buffer-name) my/global-compilation-buffer-names-list)
    (push (buffer-name) my/global-compilation-buffer-names-list)))

(add-hook 'compilation-start-hook 'my/compilation-hook)

(setq-default compilation-max-output-line-length 5000)

(setq compilation-scroll-output t)

(use-package
 emacs
 :bind
 ("<f8>" . recompile)
 ("<f9>" . project-compile)
 ("M-]" . my/next-error)
 ("M-[" . my/previous-error))

(use-package
 yasnippet
 :diminish (yas-minor-mode yas-global-mode)
 :config
 (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
 (yas-global-mode 1))

(defun my/show-buffer-diagnostics ()
  (interactive)
  (flymake-show-buffer-diagnostics)
  (message "Buffer diagnostics")
  (other-window 1))

(use-package flymake :bind ("<f5>" . my/show-buffer-diagnostics))

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(undecorated . t))

(setq-default
 left-margin-width 1
 right-margin-width 0)
(add-to-list 'default-frame-alist '(left-fringe . 0))
(add-to-list 'default-frame-alist '(right-fringe . 0))

(load-file (expand-file-name "scripts/koi-theme.el" user-emacs-directory))
(load-theme 'koi :no-confirm)

(setq-default inhibit-startup-screen t)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")

(setq-default display-line-numbers-width 3)

(use-package
 golden-ratio
 :diminish golden-ratio-mode
 :init (golden-ratio-mode 1)
 :config
 (add-hook 'ediff-startup-hook '(lambda () (golden-ratio-mode -1)) t)
 :custom
 (golden-ratio-auto-scale t)
 (golden-ratio-exclude-buffer-names '("*Occur*" "*xref*" "*Async Shell Command*")))

(set-face-attribute 'default nil
                    :font "Iosevka NF"
                    :height 130
                    :weight 'medium)
(set-face-attribute 'variable-pitch nil
                    :font "Iosevka NF"
                    :height 130
                    :weight 'medium)
(set-face-attribute 'fixed-pitch nil
                    :font "Iosevka NF"
                    :height 1.0
                    :weight 'medium)

(set-face-attribute 'font-lock-comment-face nil :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil :slant 'italic)

(add-to-list 'default-frame-alist '(font . "Iosevka NF 13"))

(setq-default line-spacing 0)

(blink-cursor-mode t)

(setq make-backup-files nil)

(use-package
 emacs
 :bind ("C-+" . text-scale-increase) ("C--" . text-scale-decrease))

(setq save-interprogram-paste-before-kill t)

(use-package
 dabbrev
 :custom
 (dabbrev-case-fold-search nil)
 (dabbrev-abbrev-char-regexp "")
 :config
 (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
 (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
 (defun my/dabbrev-select-buffer (other-buffer)
   (get-buffer-window other-buffer))
 (setq dabbrev-friend-buffer-function #'my/dabbrev-select-buffer))

(use-package hippie-exp
  :bind ("M-/" . hippie-expand)
  :config
  (load-file (expand-file-name "scripts/upcase-abbrev-expand.el" user-emacs-directory))
  (add-to-list
   'hippie-expand-try-functions-list 'try-complete-upcase-abbrev))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package
 completion
 :config
 (setq
  completions-format 'one-column
  completions-header-format nil
  completion-show-help nil)
 :bind
 (:map
  completion-in-region-mode-map
  ("C-n" . 'minibuffer-next-completion)
  ("C-p" . 'minibuffer-previous-completion)))

(use-package vertico
  :custom
  (vertico-count 13)
  (vertico-resize nil)
  (vertico-cycle nil)
  :config
  (vertico-mode))

(use-package marginalia
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'left)
  :init
  (marginalia-mode))

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))

(use-package flimenu
  :disabled
  :config
  (flimenu-global-mode))

(setq-default indent-tabs-mode nil)
(electric-indent-mode t)
(setq-default electric-indent-inhibit t)
(setq backward-delete-char-untabify-method 'hungry)

(global-display-line-numbers-mode 1)

(dolist (mode
         '(prog-mode-hook
           org-mode-hook
           magit-status-mode
           compilation-mode-hook
           conf-mode-hook
           eshell-mode-hook
           text-mode
           fundamental-mode))
  (add-hook mode 'display-line-numbers-mode))

(dolist (mode
         '(pdf-view-mode-hook
           imenu-list-minor-mode-hook imenu-list-major-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode -1))))

(setq-default display-line-numbers-type 'visual)

(setq-default scroll-margin 7)

(electric-pair-mode 1)

(menu-bar-mode -1)           ;; Disable the menu bar
(scroll-bar-mode -1)         ;; Disable the scroll bar
(tool-bar-mode -1)           ;; Disable the tool bar

(setq-default delete-selection-mode t)

(setq create-lockfiles nil)
(setq-default auto-save-default nil)

(global-auto-revert-mode t)

(setq help-window-select t)

(setq-default truncate-lines t)

(setq-default history-length 25)
(savehist-mode 1)

(save-place-mode 1)

(setq use-dialog-box nil)

(winner-mode +1) ;; Allows to restores layout after maximizing

(use-package emacs
  :bind
  ("C-," . previous-buffer)
  ("C-." . next-buffer)
  ("C-x C-b" . ibuffer)
  ("C-x k" . kill-current-buffer)
  ("C-x K" . kill-buffer))

(defun my/split-right-and-switch ()
  (interactive)
  (split-window-right)
  (windmove-right))
(window-divider-mode 1)
(use-package emacs :bind ("C-x 3" . my/split-right-and-switch))

(use-package scimotions
  :load-path (lambda () (expand-file-name "scripts/scimotions.el" user-emacs-directory)))

(use-package
 buffer-move
 :bind
 ("<C-S-up>" . buf-move-up)
 ("<C-S-down>" . buf-move-down)
 ("<C-S-left>" . buf-move-left)
 ("<C-S-right>" . buf-move-right))

(use-package
 emacs
 :bind
 ("<C-up>" . windmove-up)
 ("<C-right>" . windmove-right)
 ("<C-left>" . windmove-left)
 ("<C-down>" . windmove-down))

(defun my/scroll-half-down ()
  "Scroll down half a window."
  (interactive)
  (scroll-down (floor (/ (window-height) 2))))

(defun my/scroll-half-up ()
  "Scroll up half a window."
  (interactive)
  (scroll-up (floor (/ (window-height) 2))))

(use-package emacs
  :bind
  ("C-v" . my/scroll-half-up)
  ("M-v" . my/scroll-half-down))

(defun my/visual-inner-WORD ()
  "Select the inner word at point."
  (interactive)
  (search-backward-regexp " \\|^")
  (forward-char)
  (set-mark (point))
  (search-forward-regexp " \\|$")
  (backward-char))

(use-package emacs :bind ("C-c W" . my/visual-inner-WORD))

(use-package emacs :bind ("C-c d" . duplicate-line))

(use-package expand-region
  :bind
  ("C-;" . er/expand-region))

(setq-default async-shell-command-buffer 'new-buffer)

(setq browse-url-browser-function 'eww-browse-url)

(use-package
 sudo-edit
 :ensure t
 :config
 (defun my/sudo-edit-find-file ()
   (interactive)
   (let ((SHELL (getenv "SHELL")))
     (setenv "SHELL" "/usr/bin/bash")
     (call-interactively 'sudo-edit-find-file)
     (setenv "SHELL" SHELL))))

(defun my/display-current-time ()
  "Display the current time in the minibuffer."
  (interactive)
  (message
   (format-time-string "Current datetime: %Y-%m-%d %H:%M:%S")))

(use-package auth-source
  :custom
  (auth-sources '("~/.authinfo.gpg"))
  (auth-source-debug 'trivia)
  :config
  (auth-source-pass-enable))

(use-package free-keys
  :vc (:url "https://github.com/Fuco1/free-keys"))

(defun my/org-mode-setup ()
  (require 'org-tempo)
  (setq org-ellipsis " ▾")
  (setq org-return-follows-link t)
  (setq org-edit-src-content-indentetion 0)
  (setq-default org-edit-src-content-indentation 0) ;; Set src block automatic indent to 0 instead of 2
  (setq org-imenu-depth 4)
  (setq-default org-image-actual-width nil)
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))

(use-package org-indent
  :load-path (lambda () (expand-file-name "scripts/org-indent.el" user-emacs-directory)))

(defun my/org-mode-hook ()
  (setq org-indent-mode-turns-on-hiding-stars nil)
  (org-indent-mode)
  (set-face-attribute 'org-level-1 nil :height 1.5)
  (set-face-attribute 'org-level-2 nil :height 1.35)
  (set-face-attribute 'org-level-2 nil :height 1.2)
  (visual-line-mode 1))

(use-package
 org
 :config
 (my/org-mode-setup)
 (diminish 'org-auto-tangle-mode)
 (diminish 'org-indent-mode)
 :hook (org-mode . my/org-mode-hook)
 :bind
 (:map org-mode-map ("C-," . nil))
 ("C-c l" . org-store-link)
 ("M-n" . org-next-link)
 ("M-p" . org-previous-link)
 ("C-c a" . org-agenda)
 ("C-c t" . org-timer-set-timer))

(setq org-tag-alist
      '(("project") ("idea") ("post") ("feature") ("improve") ("bug") ("mvp") ("backlog") ("noexport")))

(use-package toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(setq org-confirm-babel-evaluate nil)

(setq org-babel-default-header-args
      '((:results . "replace")))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
   (python . t)
   (sqlite . t)
   (emacs-lisp . t)
   (plantuml . t)
   ;; (restclient . t)
   (plantuml . t)
   (awk . t)
   (sql . t)))

(use-package org-auto-tangle
  :hook (org-mode . org-auto-tangle-mode))

(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
(org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))

(defun my/org-babel-execute-all-src-blocks ()
  "Execute all source code blocks in the current Org buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp org-babel-src-block-regexp nil t)
      (org-babel-execute-src-block))))

(setq org-directory (expand-file-name "~/notes/org"))
(setq org-agenda-files (directory-files-recursively "~/notes/org/" "\\.org$"))
(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)

(setq org-todo-keywords
  '((sequence "TODO(t)" "|" "DONE(d!)")
    (sequence "TOREAD(tr)" "|" "READING(pr)" "|" "FINISED(f!")
    (sequence "INPROGRESS(p)" "INTEST(v)" "HOLD(h)" "|" "COMPLETED(c)" "CANCELED(k@)")))

(setq org-agenda-custom-commands
  '(("d" "Dashboard"
     ((agenda "" ((org-deadline-warning-days 7)))
      (todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))
      (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

    ("n" "Next Tasks"
     ((todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))))


    ("W" "Work Tasks" tags-todo "+work")

    ;; Low-effort next actions
    ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
     ((org-agenda-overriding-header "Low Effort Tasks")
      (org-agenda-max-todos 20)
      (org-agenda-files org-agenda-files)))

    ("w" "Workflow Status"
     ((todo "WAIT"
            ((org-agenda-overriding-header "Waiting on External")
             (org-agenda-files org-agenda-files)))
      (todo "REVIEW"
            ((org-agenda-overriding-header "In Review")
             (org-agenda-files org-agenda-files)))
      (todo "PLAN"
            ((org-agenda-overriding-header "In Planning")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "BACKLOG"
            ((org-agenda-overriding-header "Project Backlog")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "READY"
            ((org-agenda-overriding-header "Ready for Work")
             (org-agenda-files org-agenda-files)))
      (todo "ACTIVE"
            ((org-agenda-overriding-header "Active Projects")
             (org-agenda-files org-agenda-files)))
      (todo "COMPLETED"
            ((org-agenda-overriding-header "Completed Projects")
             (org-agenda-files org-agenda-files)))
      (todo "CANC"
            ((org-agenda-overriding-header "Cancelled Projects")
             (org-agenda-files org-agenda-files)))))))

(setq org-capture-templates
  '(    ;; ... other templates

    ("j" "Journal Entry"
         entry (file+datetree "~/journal.org")
         "* %?"
         :empty-lines 1)

        ;; ... other templates
    ))

(use-package org
 :config
 (dolist (setup
          '(("sh" . "src shell")
            ("el" . "src elisp")
            ("sq" . "src sql")
            ("sqt" . "src sql :var table=table-name")
            ("py" . "src python")
            ("pu" . "src plantuml :file ")))
   (add-to-list 'org-structure-template-alist setup)))

(use-package org-download)

(setq comint-input-ignoredups t)
(setq shell-file-name "bash")

(use-package
 eshell
 :hook
 (eshell-mode . completion-preview-mode)
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
  eshell-visual-commands '("bash" "btop" "ssh" "psql")))

(use-package eat
  :diminish
  eat-eshell-mode
  :config
  (add-hook 'eshell-mode-hook #'eat-eshell-mode)
  (add-hook 'eshell-mode-hook #'eat-eshell-visual-command-mode))

(use-package
 jinx
 :config
 (dolist (hook '(org-mode-hook conf-mode-hook))
   (add-hook hook #'jinx-mode)))

(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
      (format "%s\\|%s"
                    vc-ignore-dir-regexp

                    tramp-file-name-regexp))
(setq tramp-verbose 1)

(use-package
 gptel
 :config
 (setq
  gptel-log-level 'info
  gptel-default-mode 'org-mode
  gptel-model 'gemini-2.0-flash
  gptel-backend
  (gptel-make-gemini
   "Gemini"
   :key 'gptel-api-key-from-auth-source
   :stream t))
 (add-to-list 'gptel-directives '(frontend . "You are a senior frontend developer focused on React, TypeScript, TailwindCSS and Feature sliced design. You prefer use pnpm and biome and your main editor is GNU Emacs. Write code without comments. Answer with text only to the theoretical questions."))
 :bind ("C-c g" . gptel-menu))

(use-package
 elfeed
 :config
 (setq
  elfeed-feeds
  (quote
   (("https://www.mdpi.com/rss" research)
    ("https://protesilaos.com/interpretations.xml" philosophy)
    ("https://protesilaos.com/codelog.xml" emacs)
    ("https://pythonspeed.com/atom.xml" python)
    ("https://fabiensanglard.net/rss.xml" software)
    ("www.redblobgames.com/blog/posts.xml" math algorithms)
    ("https://www.reddit.com/r/emacsporn.rss" reddit emacs)
    ("https://opensource.com/feed" opensource linux)
    ("https://linux.softpedia.com/backend.xml" softpedia linux)
    ("https://itsfoss.com/feed/" itsfoss linux)
    ("https://www.zdnet.com/topic/linux/rss.xml" zdnet linux)
    ("https://www.computerworld.com/index.rss" computerworld linux)
    ("https://www.networkworld.com/category/linux/index.rss" networkworld linux)
    ("https://www.techrepublic.com/rssfeeds/topic/open-source/" techrepublic linux)
    ("https://systemcrafters.net/rss/news.xml" emacs)
    ("https://hnrss.org/frontpage" hackernews)
    ("http://feeds.feedburner.com/blogspot/vEnU" music jazz)
    ("https://rss.arxiv.org/rss/cs.MA" news multiagent-systems)
    ("https://www.reddit.com/r/aipromptprogramming.rss" reddit ml)
    ("https://blog.python.org/feeds/posts/default?alt=rss" python news)
    ("https://abdullin.substack.com/feed" llm)))))

(use-package
 magit
 :custom (magit-status-buffer-switch-function 'switch-to-buffer)
 (magit-display-buffer-function
  'magit-display-buffer-same-window-except-diff-v1)
 :bind ("C-x g o" . magit) ("C-x g c" . magit-commit)
 :hook (magit-status-mode-hook . display-line-numbers-mode))

(use-package
 git-gutter
 :diminish git-gutter-mode
 :custom
 (git-gutter:modified-sign "~")
 (git-gutter:added-sign "+")
 (git-gutter:deleted-sign "-")
 :config
 (defun my/stage-hunk ()
   "Wrapper around git-gutter:stage-hunk but without confirm requirement"
   (interactive)
   (git-gutter:awhen
    (git-gutter:search-here-diffinfo git-gutter:diffinfos)
    (git-gutter:do-stage-hunk it)
    (git-gutter:update-all-windows)
    (message "✅ staged" (buffer-name))))
 (add-to-list 'git-gutter:update-hooks 'focus-in-hook)
 (add-to-list 'git-gutter:update-hooks 'magit-post-refresh-hook)
 (add-to-list 'git-gutter:update-commands 'other-window)
 :bind
 ("M-)" . git-gutter:next-hunk)
 ("M-(" . git-gutter:previous-hunk)
 ("C-x g s" . my/stage-hunk)
 :hook
 ((org-mode prog-mode) . git-gutter-mode))

(use-package smerge-mode
  :diminish smerge-mode)

(defun my/ediff-hook ()
  (ediff-setup-keymap)
  (define-key ediff-mode-map "j" 'ediff-next-difference)
  (define-key ediff-mode-map "k" 'ediff-previous-difference)
  (golden-ratio-mode nil))

(use-package
 ediff
 :custom
 (ediff-split-window-function 'split-window-horizontally)
 (ediff-window-setup-function 'ediff-setup-windows-plain)
 :hook (ediff-mode . my/ediff-hook))

(defun my/dir-contains-project-marker (dir)
  "Checks if `.project' file is present in directory at DIR path."
  (let ((project-marker-path (file-name-concat dir ".project")))
    (when (file-exists-p project-marker-path)
       dir)))

(customize-set-variable 'project-find-functions
                        (list #'project-try-vc
                              #'my/dir-contains-project-marker))

(load-file (expand-file-name "scripts/my-extensions.el" user-emacs-directory))
(load-file (expand-file-name "scripts/project-ext.el" user-emacs-directory))
(require 'project-ext)

(use-package
 make-project
 :vc (:url "https://github.com/scipunch/make-project")
 :bind ("C-x p c" . make-project-run))

(defun my/project-or-default-eshell ()
  "Open eshell in project root or in the current."
  (interactive)
  (if (project-current)
      (project-eshell)
    (eshell)))

(use-package project
  :custom
  (project-mode-line t)
  :config
  (defun my/project-switch ()
    (interactive)
    (let ((project-dir (project-prompt-project-dir)))
      (setq-local project-current-directory-override project-dir)
      (project-find-file)))
  :bind
  ("C-x p e" . my/project-or-default-eshell)
  ("C-x p F" . project-root-find-file)
  ("C-x p p" . my/project-switch))

(use-package dired-open
  :custom ((dired-listing-switches "-agho --group-directories-first")
           (dired-kill-when-opening-new-dired-buffer t))
  :config
  (setq dired-open-extensions '(("gif" . "feh")
                                ("jpg" . "feh")
                                ("jpeg" . "feh")
                                ("png" . "feh")
                                ("mkv" . "mpv")
                                ("mp4" . "mpv"))))

(use-package graphql-mode :disabled)
(use-package graphql :disabled)
(use-package request :disabled)

(use-package eaf
  :disabled
  :load-path "~/.emacs.d/site-lisp/emacs-application-framework"
  :config
  (setq eaf-python-command (expand-file-name "site-lisp/.venv/bin/python3" user-emacs-directory)))

(use-package eaf-browser
  :disabled
  :after eaf
  :load-path "~/.emacs.d/site-lisp/eaf-browser"
  :custom
  (eaf-browser-enable-adblocker t)
  :config
  (defalias 'browse-web #'eaf-open-browser))

(use-package pdf-tools)

(defun my/async-shell-command-on-region ()
  (interactive)
  (async-shell-command (buffer-substring (region-beginning) (region-end))))

(use-package
 eww-ext
 :load-path
 (lambda ()
   (expand-file-name "scripts/eww-ext.el" user-emacs-directory))
 :custom
 (eww-ext:search-engines
  '(("pyTelegramBotAPI docs"
     .
     "https://pytba.readthedocs.io/en/latest/search.html?q=%s&check_keywords=yes&area=default"))))

(message "Config fully loaded")
