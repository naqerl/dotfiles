(add-to-list 'load-path "~/dotfiles/.emacs.d/scripts")
(add-to-list 'load-path "~/dotfiles/.emacs.d/themes")
(require 'package-manager)
(require 'suzu-extensions)

(use-package gcmh
  :init
  (gcmh-mode 1))

(setq-default debug-on-error nil)

(require 'pyright-write)
(require 'suzu-buffer)
(require 'suzu-project)

(use-package async :ensure t)
(require 'ob-async-sql)

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-window-right t)
  (setq evil-want-C-w-delete t)
  (setq evil-want-window-below t)
  (setq evil-want-C-u-scroll t)
  (setq evil-undo-system 'undo-redo)
  :config
  (evil-mode 1))

(use-package undo-tree :ensure t)

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "SPC") nil)
  (define-key evil-motion-state-map (kbd "RET") nil))
(setq org-return-follows-link t)

(defalias 'forward-evil-word 'forward-evil-symbol)

(with-eval-after-load 'evil-maps
  (define-key evil-outer-text-objects-map "w" 'evil-a-symbol)
  (define-key evil-inner-text-objects-map "w" 'evil-inner-symbol)
  (define-key evil-outer-text-objects-map "o" 'evil-a-word)
  (define-key evil-inner-text-objects-map "o" 'evil-inner-word))

(use-package better-jumper
  :diminish
  :config
  (better-jumper-mode +1))

(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "C-o") 'better-jumper-jump-backward)
  (define-key evil-motion-state-map (kbd "C-i") 'better-jumper-jump-forward))

(defun suzu/window-left ()
  (interactive)
  (evil-window-left 1)
  (golden-ratio))

(defun suzu/window-down ()
  (interactive)
  (evil-window-down 1)
  (golden-ratio))

(defun suzu/window-up ()
  (interactive)
  (evil-window-up 1)
  (golden-ratio))

(defun suzu/window-right ()
  (interactive)
  (evil-window-right 1)
  (golden-ratio))

(with-eval-after-load 'evil-maps
  (define-key evil-normal-state-map (kbd "C-h") 'suzu/window-left)
  (define-key evil-normal-state-map (kbd "C-j") 'suzu/window-down)
  (define-key evil-normal-state-map (kbd "C-k") 'suzu/window-up)
  (define-key evil-normal-state-map (kbd "C-l") 'suzu/window-right))

(defun suzu/split-window-vertical()
  (interactive)
  (split-window-right)
  (other-window 1))

(defun suzu/split-window-horizontal()
  (interactive)
  (split-window-below)
  (other-window 1))

(use-package general
  :config
  (general-evil-setup)

  (general-define-key
   :states 'normal
   :prefix "g"
   "n" '(suzu/buffer-next :wk "Next buffer")
   "p" '(suzu/buffer-prev :wk "Previous buffer"))

  (general-define-key
   :states 'normal
   :prefix "C-w"
   "v" '(suzu/split-window-vertical :wk "Vertical split")
   "s" '(suzu/split-window-horizontal :wk "Horizontal split"))

  (general-define-key
   :states '(normal visual insert)
   "<f2>" '(vterm-toggle-forward :wk "Toggle vterm forward")
   "<f3>" '(eshell-toggle :wk "Toggle eshell")
   "<f4>" '(vterm-toggle-backward :wk "Toggle vterm backward"))

  (general-define-key
   :states '(normal visual)
   "[ g" '(git-gutter:previous-hunk :wk "Prev git hunk")
   "] g" '(git-gutter:next-hunk :wk "Next git hunk")
   "[ d" '(flymake-goto-prev-error :wk "Prev diagnostic")
   "] d" '(flymake-goto-next-error :wk "Next diagnostic"))

  (general-create-definer suzu/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-M-SPC")

  (suzu/leader-keys
    "." '(ido-find-file :wk "Find file")
    ";" '(execute-extended-command :wk "M-x")
    "'" '(consult-ripgrep :wk "Ripgrep project symbols")
    "i" '(consult-imenu :wk "Open imenu")
    "P" '(suzu/project-switch-in-new-perspective :wk "Open project in new perspective")
    "B" '(consult-project-buffer :wk "Switch buffer in perspective")
    "S" '(persp-switch :wk "Switch perspective")
    "l" '(persp-switch-last :wk "Switch last perspective")
    "/" '(consult-line :wk "Search in buffer")
    "f" '(project-find-file :wk "Find file"))

  (suzu/leader-keys
    "s" '(:ignore t :wk "Session")
    "s b" '(bufler-switch-buffer :wk "Switch buffer")
    "s k" '(persp-kill :wk "Kill perspective")
    "s p" '(persp-prev :wk "Prev session")
    "s n" '(persp-next :wk "Next session"))

  (suzu/leader-keys
    "b" '(:ignore t :wk "buffer || bookmark")
    "b I" '(ibuffer :wk "Ibuffer")
    "b i" '(persp-ibuffer :wk "Perspective ibuffer")
    "b s" '(consult-buffer :wk "Search buffer")
    "b k" '(suzu/kill-current-buffer :wk "Kill this buffer")
    "b r" '(revert-buffer :wk "Reload buffer")
    "b m" '(bookmark-set :wk "Bookmark")
    "b l" '(list-bookmarks :wk "Bookmarks list"))

  (suzu/leader-keys
    "e" '(:ignore t :wk "Evaluate")
    "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
    "e d" '(eval-defun :wk "Evaluate defun containing or after point")
    "e e" '(eval-expression :wk "Evaluate and elisp expression")
    "e l" '(eval-last-sexp :wk "Evaluate elisp expression before point")
    "e r" '(eval-region :wk "Evaluate elisp in region"))

  (suzu/leader-keys
    "g" '(:ginore t :wk "Git")
    "g p" '((lambda () (interactive) (git-gutter:popup-hunk) (other-window 1)) :wk "Preview hunk diff")
    "g r" '(git-gutter:revert-hunk :wk "Preview hunk diff")
    "g w" '(magit-worktree :wk "Git worktree")
    "g s" '(git-gutter:stage-hunk :wk "Preview hunk diff"))

  (suzu/leader-keys
    "o" '(:ignore t :wk "Open")
    "o r" '(consult-recent-file :wk "Open recent files")
    "o E" '(dired-jump :wk "Dired jump to current")
    "o e" '(project-dired :wk "Project root dired")
    "o p d" '(peep-dired :wk "Peep-dired")
    "o s" '(eshell :wk "Open eshell")
    "o g" '(magit :wk "Open magit")
    "o d" '((lambda () (interactive) (flymake-show-buffer-diagnostics) (message "Buffer diagnostics") (other-window 1)) :wk "Open buffer diagnostics")
    "o D" '((lambda () (interactive) (flymake-show-project-diagnostics) (message "Project diagnostics") (other-window 1)) :wk "Open project diagnostics")
    "o t" '(multi-vterm :wk "Open Vterm")
    "o c" '((lambda ()
              (interactive)
              (persp-switch "dotfiles")
              (project-switch-project "~/dotfiles/")) :wk "Edit emacs config"))

  (suzu/leader-keys
    "h" '(:ignore t :wk "Help")
    "h f" '(describe-function :wk "Describe function")
    "h v" '(describe-variable :wk "Describe variable")
    "h M" '(info-display-manual :wk "Manual")
    "h m" '(describe-mode :wk "Describe mode")
    "h p" '(describe-package :wk "Describe package")
    "h r r" '((lambda ()
                (interactive)
                (load-file "~/dotfiles/.emacs.d/init.el")) :wk "Reload emacs config"))

  (suzu/leader-keys
    "m" '(:ignore t :wk "Org")
    "m a" '(org-agenda :wk "Org agenda")
    "m o" '(org-open-at-point :wk "Org open at point")
    "m e" '(org-babel-async-execute-sql :wk "Execute org babel src block")
    "m i" '(org-toggle-item :wk "Org toggle item")
    "m I" '(org-toggle-inline-images :wk "Org toggle inline images")
    "m t" '(org-todo :wk "Org todo")
    "m B" '(org-babel-tangle :wk "Org babel tangle")
    "m l" '(org-insert-link :wk "Org insert link")
    "m T" '(org-todo-list :wk "Org todo list"))

  (suzu/leader-keys
    "m b" '(:ignore t :wk "Tables")
    "m b -" '(org-table-insert-hline :wk "Insert hline in table")
    "m d" '(:ignore t :wk "Date/deadline")
    "m d t" '(org-time-stamp :wk "Org time stamp")
    "m r f" '(org-roam-node-find :wk "Org Roam find node")
    "m r b" '(org-roam-buffer-toggle :wk "Org Roam show backlinks")
    "m r i" '(org-roam-node-insert :wk "Org Roam insert node"))

  (suzu/leader-keys
    "c a" '(eglot-code-actions :wk "Code actions")
    "r n" '(eglot-rename :wk "Rename"))

  (suzu/leader-keys
    "t" '(:ignore t :wk "Toggle")
    "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
    "t i" '(eglot-inlay-hints-mode :wk "Toggle inlay hints")
    "t c" '(suzu/center-buffer :wk "Center buffer")
    "t t" '(visual-line-mode :wk "Toggle truncated lines"))

  (suzu/leader-keys
    "s" '(:ignore t :wk "Slack")
    "s m" '(slack-im-select :wk "Select instant message")
    "s c" '(slack-channel-select :wk "Select channel")
    "s t" '(slack-all-threads :wk "List threads")
    "s f" '(slack-search-from-messages :wk "Find message")
    "s F" '(slack-search-from-files :wk "Find file"))
  )

(setq modus-themes-mode-line '(borderless 3d)
      modus-themes-region '(bg-only)
      modus-themes-org-blocks 'gray-background
      modus-themes-completions '((selection intense) (popup intense))
      modus-themes-bold-constructs t
      modus-themes-italic-constructs t
      modus-themes-paren-match '(bold)
      modus-themes-fringes nil
      modus-themes-syntax '(green-strings yellow-comments)
      modus-themes-headings '((0 . (rainbow 1.9))
        (1 . (rainbow 1.5))
        (2 . (rainbow 1.3))
        (3 . (rainbow 1.2))
        (t . (semilight 1.1 ))))
(load-theme 'modus-vivendi :no-confirm)

(setq-default display-line-numbers-width 4)

(use-package auto-dim-other-buffers
  :disabled
  :custom
  (auto-dim-other-buffers-dim-on-switch-to-minibuffer nil)
  (auto-dim-other-buffers-affected-faces '((default . auto-dim-other-buffers-face)
                                           (org-hide . auto-dim-other-buffers-hide-face))))

(use-package golden-ratio
  :init
  (golden-ratio-mode 1)
  :custom
  (golden-ratio-auto-scale t))

(use-package zen-mode
  :ensure t)

(use-package auth-source
  :config
  (auth-source-pass-enable)
  (setq auth-source-debug 'trivia))

(use-package magit
  :config
  (setq magit-status-buffer-switch-function 'switch-to-buffer)
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))

(use-package git-gutter
  :custom
  (git-gutter:modified-sign " ") ;; two space
  (git-gutter:added-sign " ")    ;; multiple character is OK
  (git-gutter:deleted-sign " ")
  :config
  (global-git-gutter-mode +1))

(setq-default mode-line-format nil)

(use-package all-the-icons
  :ensure t)
(use-package all-the-icons-dired
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

(setq make-backup-files nil)

(use-package corfu
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto nil)                 ;; Enable auto completion
  (corfu-auto-delay 1)
  (corfu-auto-prefix 2)
  (corfu-popupinfo-mode t)
  :init
  (global-corfu-mode))

(defun corfu-enable-always-in-minibuffer ()
  "Enable Corfu in the minibuffer if Vertico/Mct are not active."
  (unless (or (bound-and-true-p mct--active)
              (bound-and-true-p vertico--input)
              (eq (current-local-map) read-passwd-map))
    (setq-local corfu-auto nil) ;; Enable/disable auto completion
    (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                corfu-popupinfo-delay nil)
    (corfu-mode 1)))
(add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)

(use-package emacs
  :init
  (setq completion-cycle-threshold 3))

(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  ;; Since 29.1, use `dabbrev-ignored-buffer-regexps' on older.
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package nerd-icons-corfu
:config
(add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package dired-open
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (evil-define-key 'normal dired-mode-map (kbd "h") 'dired-up-directory)
  (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-open-file)
  (setq dired-open-extensions '(("gif" . "feh")
                                ("jpg" . "feh")
                                ("jpeg" . "feh")
                                ("png" . "feh")
                                ("mkv" . "mpv")
                                ("mp4" . "mpv"))))

(use-package peep-dired
  :after dired
  :hook (evil-normalize-keymaps . peep-dired-hook)
  )

(setf dired-kill-when-opening-new-dired-buffer t)
(setq-default dired-listing-switches "-aBhl  --group-directories-first")

(use-package elfeed
  :config
  (setq elfeed-search-feed-face ":foreground #ffffff :weight bold"
        elfeed-feeds (quote
                      (
                       ("https://www.reddit.com/r/emacsporn.rss" reddit)
                       ("https://www.gamingonlinux.com/article_rss.php" gaming linux)
                       ("https://hackaday.com/blog/feed/" hackaday linux)
                       ("https://opensource.com/feed" opensource linux)
                       ("https://linux.softpedia.com/backend.xml" softpedia linux)
                       ("https://itsfoss.com/feed/" itsfoss linux)
                       ("https://www.zdnet.com/topic/linux/rss.xml" zdnet linux)
                       ("https://www.computerworld.com/index.rss" computerworld linux)
                       ("https://www.networkworld.com/category/linux/index.rss" networkworld linux)
                       ("https://www.techrepublic.com/rssfeeds/topic/open-source/" techrepublic linux)
                       ("https://betanews.com/feed" betanews linux)
                       ("https://systemcrafters.net/rss/news.xml" emacs)
                       ("https://hnrss.org/frontpage" hackernews)
                       ("http://feeds.feedburner.com/blogspot/vEnU" music jazz)))))


(use-package elfeed-goodies
  :init
  (elfeed-goodies/setup)
  :config
  (setq elfeed-goodies/entry-pane-size 0.5))

(add-hook 'elfeed-show-mode-hook 'visual-line-mode)

(set-face-attribute 'default nil
                    :font "iosevka NF"
                    :height 130
                    :weight 'medium)
(set-face-attribute 'variable-pitch nil
                    :font "Iosevka NF"
                    :height 130
                    :weight 'medium)
(set-face-attribute 'fixed-pitch nil
                    :font "Iosevka NF"
                    :height 130
                    :weight 'medium)
(set-face-attribute 'font-lock-comment-face nil
                    :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
                    :slant 'italic)

(add-to-list 'default-frame-alist '(font . "Iosevka NF 13"))
(setq default-frame-alist '((font . "Iosevka NF 13")))

(setq-default line-spacing 0)

(setq ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

(defun suzu/ediff-hook ()
(ediff-setup-keymap)
(define-key ediff-mode-map "j" 'ediff-next-difference)
(define-key ediff-mode-map "k" 'ediff-previous-difference))

(add-hook 'ediff-mode-hook 'suzu/ediff-hook)

(use-package evil-nerd-commenter
  :config
  (general-define-key
   :states 'normal
   :prefix "g"
   "c" '(evilnc-comment-or-uncomment-lines :wk "Comment lines")))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (setq dashboard-display-icons-p t)
  (setq dashboard-path-max-length 10)
  (setq dashboard-vertically-center-content nil)
  :custom
  (dashboard-startup-banner "/home/suzu/.emacs.d/images/hydra.txt")
  (dashboard-center-content t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t))

(use-package eldoc-box
  :config
  (defun suzu/eldoc-box-scroll-up ()
    "Scroll up in `eldoc-box--frame'"
    (interactive)
    (with-current-buffer eldoc-box--buffer
      (with-selected-frame eldoc-box--frame
        (scroll-down 3))))
  (defun suzu/eldoc-box-scroll-down ()
    "Scroll down in `eldoc-box--frame'"
    (interactive)
    (with-current-buffer eldoc-box--buffer
      (with-selected-frame eldoc-box--frame
        (scroll-up 3))))
  (setq max-mini-window-height 0)
  (setq eldoc-idle-delay 0)
  (general-define-key
   :states '(normal visual motion)
   :keymaps 'override
   "K" '(eldoc-box-help-at-point :wk "Show doc")))

(defun suzu/rust-mode()
  (eglot-ensure))

(use-package rust-mode
  :init
  (setq rust-mode-treesitter-derive t)
  (setq rust-format-on-save t))
(add-hook 'rust-mode-hook 'suzu/rust-mode)

(defun suzu/python-mode()
  (add-hook 'before-save-hook 'python-sort-imports nil t)
  (eglot-ensure))

(use-package python
  :hook
  (python-ts-mode . suzu/python-mode)
  (python-ts-mode . python-black-on-save-mode))

(use-package python-black
  :ensure t)

(use-package yuck-mode
  :ensure t)

(use-package sqlformat
:config
(setq sqlformat-command 'pgformatter)
(setq sqlformat-args '("-s2" "-g"))
:hook
(sql-mode-hook . sqlformat-on-save-mode))

(use-package markdown-mode
  :ensure t)

(use-package csv-mode
  :ensure t)

(use-package typescript-mode)

(defun suzu/format-elisp-on-save ()
  (add-hook 'before-save-hook 'elisp-autofmt-buffer nil t))
(add-hook 'emacs-lisp-mode-hook 'suzu/format-elisp-on-save)

(setq read-process-output-max (* 1024 1024))

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '(rust-ts-mode . ("rust-analyzer")))
  (add-to-list 'eglot-server-programs '(python-mode . ("pyright"))))

(use-package dap-mode
  :ensure t)

(setq-default indent-tabs-mode nil)
(electric-indent-mode t)
(setq-default electric-indent-inhibit t)
(setq backward-delete-char-untabify-method 'hungry)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'org-mode-hook 'display-line-numbers-mode)
(dolist (mode '(pdf-view-mode-hook
                term-mode-hook
                eshell-mode-hook
                vterm-mode-hook
                imenu-list-minor-mode-hook
                imenu-list-major-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode -1))))
(setq-default display-line-numbers-type 'relative)

(setq-default scroll-margin 7)

(electric-pair-mode 1)

(menu-bar-mode -1)           ;; Disable the menu bar
(scroll-bar-mode -1)         ;; Disable the scroll bar
(tool-bar-mode -1)           ;; Disable the tool bar

(setq create-lockfiles nil)
(setq-default auto-save-default nil)

(global-auto-revert-mode t)

(setq help-window-select t)

(setq-default truncate-lines t)

(use-package emojify
  :ensure t)

(with-eval-after-load 'org
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src elisp"))
  (add-to-list 'org-structure-template-alist '("sq" . "src sql"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))

(add-hook 'org-mode-hook
  (lambda ()
    (setq-local electric-pair-inhibit-predicate
      `(lambda (c)
        (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))

(defun suzu/visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :config
  :hook
  (org-mode . suzu/visual-fill)
  (dired-mode . suzu/visual-fill)
  (eshell-mode . suzu/visual-fill)
  (term-mode . suzu/visual-fill)
  (shell-mode . suzu/visual-fill)
  (prog-mode . suzu/visual-fill)
  (text-mode . suzu/visual-fill))

(defun suzu/org-mode-setup ()
  (setq org-ellipsis " ▾")
  (setq org-edit-src-content-indentetion 0)
  (setq-default org-edit-src-content-indentation 0) ;; Set src block automatic indent to 0 instead of 2
  (setq org-imenu-depth 4)
  (setq-default org-image-actual-width nil)
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))

(defun suzu/org-icons ()
  "Beautify org mode keywords."
  (setq prettify-symbols-alist '(("TODO" . "")
                                 ("WAIT" . "")        
                                 ("NOPE" . "")
                                 ("DONE" . "")
                                 ("[#A]" . "")
                                 ("[#B]" . "")
                                 ("[#C]" . "")
                                 ("[ ]" . "")
                                 ("[X]" . "")
                                 ("[-]" . "")
                                 ("#+begin_src" . "")
                                 ("#+end_src" . "")
                                 (":properties:" . "")
                                 (":PROPERTIES:" . "")
                                 (":end:" . "―")
                                 (":END:" . "―")
                                 (":ID:" . "")
                                 ("#+startup:" . "")
                                 ("#+title: " . "")
                                 ("#+results:" . "")
                                 ("#+name:" . "")
                                 ("#+roam_tags:" . "")
                                 ("#+filetags:" . "")
                                 ("#+html_head:" . "")
                                 ("#+subtitle:" . "")
                                 ("#+author:" . "")
                                 ("#+description:" . "󰦨")
                                 (":effort:" . "")
                                 ("*" . "󰣏")
                                 ("**" . " 󱀝")
                                 ("***" . "  ")
                                 ("****" . "   ")
                                 ("*****" . "    ")
                                 ("******" . "     ")
                                 ("scheduled:" . "")
                                 ("#+auto_tangle: t" . "󰁪")
                                 ("deadline:" . "")))
  (prettify-symbols-mode))

(defun suzu/org-mode-hook ()
  (require 'suzu-org-indent)
  (setq org-indent-mode-turns-on-hiding-stars nil)
  (org-indent-mode)
  (suzu/org-icons)
  (evil-define-key '(normal) org-mode-map (kbd "C-k") 'suzu/window-up)
  (evil-define-key '(normal) org-mode-map (kbd "C-j") 'suzu/window-down)
  (visual-line-mode 1))

(use-package org
  :pin org
  :commands (org-capture org-agenda)
  :config
  (suzu/org-mode-setup)
  :hook (org-mode . suzu/org-mode-hook))

(use-package toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(setq org-confirm-babel-evaluate nil)

(setq-default plantuml-exec-mode "plantuml")

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
   (python . t)
   (sqlite . t)
   (emacs-lisp . t)
   (plantuml . t)
   (restclient . t)
   (sql . t)))

(use-package org-auto-tangle
  :hook (org-mode . org-auto-tangle-mode))

(defun suzu/run-after-tangle-hook ()
    (add-hook 'org-bable-tangle-finished-hook (lambda () (org-babel-ref-resolve "run-after-save"))))

;; (add-hook 'org-mode-hook 'suzu/org-babel-run-after-save-hook)

(use-package org-roam
  :config
  (org-roam-db-autosync-mode))

(setq org-directory "~/org-roam")
(setq org-agenda-files '("tasks.org" "habits.org"))
(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)

(setq org-todo-keywords
  '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
    (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

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

(defun suzu/pdf-setup-hook ()
  (setq blink-cursor-mode nil))

(use-package pdf-tools
  :config
  (pdf-tools-install))

(add-hook 'pdf-view-mode-hook 'suzu/pdf-setup-hook)

(defun suzu/find-pdf-file ()
  (interactive)
  (let* ((places '("~/Downloads" "~/Documents/books"))
         (files-from-places (mapcar (lambda (place) (directory-files place t "\\.pdf$")) places))
         (files (suzu/flatten-list files-from-places))
         (file (completing-read "Choose PDF file: " files)))
    (find-file file)))

(defun suzu/dir-contains-project-marker (dir)
  "Checks if `.project' file is present in directory at DIR path."
  (let ((project-marker-path (file-name-concat dir ".project")))
    (when (file-exists-p project-marker-path)
       dir)))

(customize-set-variable 'project-find-functions
                        (list #'project-try-vc
                              #'suzu/dir-contains-project-marker))

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

(use-package vertico
  :custom
  (vertico-count 13)
  (vertico-resize nil)
  (vertico-cycle nil)
  :config
  (vertico-mode))

(use-package consult)

(use-package rainbow-mode
  :diminish
  :hook
  ((org-mode prog-mode) . rainbow-mode))

(setq comint-input-ignoredups t)

(use-package eshell-git-prompt
  :ensure t)

(setq suzu/eshell-aliases
      '((g  . magit)
        (gl . magit-log)
        (d  . dired)
        (o  . find-file)	
        (clc  . eshell/clear-scrollback)	
        (oo . find-file-other-window)))

(mapc (lambda (alias)
        (defalias (car alias) (cdr alias)))
      suzu/eshell-aliases)

(defun suzu/configure-eshell ()
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'consult-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-k") 'suzu/window-up)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-j") 'suzu/window-down)
  (add-hook 'evil-insert-state-entry-hook '(lambda () (setq display-line-numbers nil)) nil t)
  (add-hook 'evil-normal-state-entry-hook '(lambda () (display-line-numbers-mode 1) (setq display-line-numbers-type 'relative)) nil t)
  (visual-line-mode)
  (evil-normalize-keymaps))

(use-package eshell
  :hook (eshell-first-time-mode . suzu/configure-eshell)
  :config
  ;; (eshell-git-prompt-use-theme 'powerline)
  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t
        eshell-rc-script (concat user-emacs-directory "eshell/profile")
        eshell-aliases-file (concat user-emacs-directory "eshell/aliases")
        eshell-destroy-buffer-when-process-dies t
        ;; eshell-prompt-function 'suzu/eshell-prompt
        ;; eshell-prompt-regexp suzu/eshell-prompt-regexp
        eshell-prompt-function (lambda () "A simple prompt." "󰘧 ")
        eshell-prompt-regexp "^󰘧 "
        eshell-visual-commands '("bash" "fish" "htop" "ssh" "top" "zsh" "paru")))

(use-package eshell-syntax-highlighting
  :config
  (eshell-syntax-highlighting-global-mode +1))

(use-package eshell-toggle
  :custom
  (eshell-toggle-window-side 'above)
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-projectile-root nil)
  (eshell-toggle-use-git-root t)
  (eshell-toggle-run-command nil))

(use-package pcmpl-args
  :ensure t)

(defun eshell/asc (cmd)
  "Runs `cmd` in async bash shell"
  (async-shell-command (format "bash -c '%s'" cmd)))
(put 'eshell/asc 'eshell-no-numeric-conversions t)

(use-package vterm
  :config
  (setq vterm-shell "/usr/bin/bash"
        vterm-buffer-name-string "vterm %s"
        vterm-max-scrollback 5000)
  (defun get-full-list ()
    (let ((program-list (process-lines "bash" "-c" "compgen -c"))
          (file-directory-list (process-lines "bash" "-c" "compgen -f"))
          (history-list (with-temp-buffer
                          (insert-file-contents "~/.bash_history")
                          (split-string (buffer-string) "\n" t))))

      (delete-dups (append program-list file-directory-list history-list))))

  (defun vterm-completion-choose-item ()
    (completing-read "Choose: " (get-full-list) nil nil (thing-at-point 'word 'no-properties)))

  (defun vterm-completion ()
    (interactive)
    (vterm-directory-sync)
    (setq vterm-chosen-item (vterm-completion-choose-item))
    (when (thing-at-point 'word)
      (vterm-send-meta-backspace))
    (vterm-send-string vterm-chosen-item))

  (defun vterm-directory-sync ()
    "Synchronize current working directory."
    (interactive)
    (when vterm--process
      (let* ((pid (process-id vterm--process))
             (dir (file-truename (format "/proc/%d/cwd/" pid))))
        (setq default-directory dir))))

  ;; :general
  ;; (:states 'insert
  ;;          :keymaps 'vterm-mode-map
  ;;          "<tab>" 'vterm-completion)
)

(use-package vterm-toggle
  :after vterm
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (setq vterm-toggle-scope 'project)
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 ;;(display-buffer-reuse-window display-buffer-in-direction)
                 ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                 ;;(direction . bottom)
                 ;;(dedicated . t) ;dedicated is supported in emacs27
                 (reusable-frames . visible)
                 (window-height . 0.3))))

(use-package multi-vterm
  :config
  (add-hook 'vterm-mode-hook
            (lambda ()
              (setq-local evil-insert-state-cursor 'box)
              (evil-insert-state)))
  (define-key vterm-mode-map [return]                      #'vterm-send-return))

(use-package sudo-edit
  :config
  (suzu/leader-keys
    "o w s" '(sudo-edit :wk "Sudo edit file")))

(use-package tldr :ensure t)

(add-to-list 'default-frame-alist '(alpha-background . 60))
(add-to-list 'corfu--frame-parameters '(alpha-background . 100))

(setq treesit-language-source-alist
      '((rust "https://github.com/tree-sitter/tree-sitter-rust")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript")
        (c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp")))

(setq treesit-font-lock-level 4)
(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)
        (rust-ts-mode . rust-mode)
        (typescript-ts-mode . typescript-mode)))

(use-package which-key
  :diminish
  :init
  (which-key-mode)
  :config
  (setq which-key-popup-type 'side-window
        which-key-side-window-max-height 0.50))

(use-package perspective
  :init
  (setq persp-suppress-no-prefix-key-warning t)
  (persp-mode)
  :config
  (persp-turn-off-modestring))

(add-hook 'ibuffer-hook
          (lambda ()
            (persp-ibuffer-set-filter-groups)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic))))

;; (add-hook 'kill-emacs-hook #'persp-state-save)

(use-package bufler
  :ensure t)

(defun suzu/update-eww-var (var value)
  (call-process "eww" nil nil nil "update" (format "%s=%s" var value)))

(defun suzu/current-perspective ()
  (suzu/update-eww-var "emacs_session" (persp-current-name)))

(add-hook 'persp-switch-hook 'suzu/current-perspective)

(defun suzu/current-window ()
  (suzu/update-eww-var "emacs_window_icon" (nerd-icons-icon-for-buffer))
  (suzu/update-eww-var "emacs_window" (buffer-name)))

(add-hook 'window-state-change-hook 'suzu/current-window)

(defun suzu/current-buffer-saved ()
  (if (and (buffer-modified-p) (not buffer-read-only))
      (suzu/update-eww-var "emacs_buffer_modifier" " ")
      (suzu/update-eww-var "emacs_buffer_modifier" "")))

(add-hook 'evil-normal-state-entry-hook 'suzu/current-buffer-saved)
(add-hook 'window-state-change-hook 'suzu/current-buffer-saved)
(add-hook 'after-save-hook 'suzu/current-buffer-saved)

(defun suzu/current-vcs-branch ()
  (suzu/update-eww-var "git_branch" (magit-get-current-branch)))

;; (add-hook 'find-file-hook 'suzu/current-vcs-branch)
;; (add-hook 'after-save-hook 'suzu/current-vcs-branch)

(defun suzu/lsp-status ()
  (if (eglot-current-server)
    (suzu/update-eww-var "emacs_lsp" " ")
    (suzu/update-eww-var "emacs_lsp" "")))

(add-hook 'eglot-managed-mode-hook 'suzu/lsp-status)
(add-hook 'find-file-hook 'suzu/lsp-status)
(add-hook 'persp-switch-hook 'suzu/lsp-status)

(use-package slack
  :disabled
  :ensure (:repo "https://github.com/yuya373/emacs-slack")
  ;; :commands (slack-start)
  :init
  (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
  (setq slack-prefer-current-team t)
  :config
  (slack-register-team
   :default t
   :name "pixelplex"
   :token (auth-source-pass--read-entry  "slack.com/token")
   :cookie (auth-source-pass--read-entry  "slack.com/cookie")))

(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'libnotify))

(use-package telega)

(use-package gptel
  :disabled
  :config
  (setq
   gptel-log-level 'info
   gptel-model "claude-3-sonnet-20240229" ;  "claude-3-opus-20240229" also available
   gptel-backend (gptel-make-anthropic "Claude"
                                       :stream t :key (auth-source-pass--read-entry  "anthropic.com/apikey"))))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

(use-package enwc
:custom (enwc-default-backend 'nm))

(defun suzu/launch-linux-app ()
  "Select and launch a Linux application using Vertico."
  (interactive)
  (let* ((app (completing-read "󰅂 "
                               (directory-files "/usr/share/applications" t "\\.desktop$")))
         (app-filename (file-name-nondirectory app))
         (app-name (cl-subseq app-filename 0 (- (length app-filename) 8))))
    (if (and app (file-exists-p app))
        (start-process "linux-app-launcher" "gtk-launch" app-name)
      (message "Invalid application selected."))))

(defun suzu/emacs-app-launcher ()
  (interactive)
  (with-selected-frame
      (make-frame '((name . "emacs-run-launcher")
                    (minibuffer . only)
                    (fullscreen . 0)
                    (undecorated . t)
                    (internal-border-width . 10)
                    (width . 80)
                    (height . 10)))
    (unwind-protect
        (suzu/launch-linux-app)
        (delete-frame))))

(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
      (format "%s\\|%s"
                    vc-ignore-dir-regexp
                    tramp-file-name-regexp))
(setq tramp-verbose 1)

(use-package restclient)
(use-package ob-restclient)
