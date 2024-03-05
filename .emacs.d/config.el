(add-to-list 'load-path "~/dotfiles/.emacs.d/scripts")
(add-to-list 'load-path "~/dotfiles/.emacs.d/themes")
(require 'elpaca-setup)
;; (require 'no-littering)

(use-package gcmh
  :ensure t
  :init
  (gcmh-mode 1))

(setq-default debug-on-error nil)

(require 'pyright-write)
(require 'suzu-buffer)

(use-package async :ensure t)
(require 'ob-async-sql)

(use-package evil
  :ensure t
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
  :ensure t
  :config
  (evil-collection-init))

(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "SPC") nil)
  (define-key evil-motion-state-map (kbd "RET") nil))
(setq org-return-follows-link  t)

(defalias 'forward-evil-word 'forward-evil-symbol)

(with-eval-after-load 'evil-maps
  (define-key evil-outer-text-objects-map "w" 'evil-a-symbol)
  (define-key evil-inner-text-objects-map "w" 'evil-inner-symbol)
  (define-key evil-outer-text-objects-map "o" 'evil-a-word)
  (define-key evil-inner-text-objects-map "o" 'evil-inner-word))

(use-package better-jumper
  :ensure t
  :diminish
  :config
  (better-jumper-mode +1))

(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "C-o") 'better-jumper-jump-backward)
  (define-key evil-motion-state-map (kbd "C-i") 'better-jumper-jump-forward))

(defun suzu/split-window-vertical()
(interactive)
(split-window-right)
(other-window 1))

(defun suzu/split-window-horizontal()
(interactive)
(split-window-below)
(other-window 1))

(use-package general
  :ensure t
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
   :states '(normal visual)
   "<f2>" '(vterm-toggle-forward :wk "Toggle vterm forward")
   "<f3>" '(vterm-toggle :wk "Toggle vterm")
   "<f4>" '(vterm-toggle-backward :wk "Toggle vterm backward")
   "[ g" '(git-gutter:previous-hunk :wk "Prev git hunk")
   "] g" '(git-gutter:next-hunk :wk "Next git hunk")
   "[ d" '(flymake-goto-prev-error :wk "Prev diagnostic")
   "] d" '(flymake-goto-next-error :wk "Next diagnostic"))

  (general-create-definer suzu/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-SPC")

  (suzu/leader-keys
    "." '(find-file :wk "Find file")
    ";" '(counsel-M-x :wk "Counsel M-x")
    "'" '(counsel-projectile-rg :wk "Ripgrep project symbols")
    "i" '(counsel-imenu :wk "Open imenu")
    "P" '(projectile-persp-switch-project :wk "Open project in new perspective")
    "S" '(persp-switch :wk "Switch perspective")
    "l" '(persp-switch-last :wk "Switch last perspective")
    "f" '(projectile-find-file :wk "Find file"))

  (suzu/leader-keys
    "s" '(:ignore t :wk "Session")
    "s b" '(bufler-switch-buffer :wk "Switch buffer")
    "s k" '(persp-kill :wk "Kill perspective")
    "s p" '(persp-prev :wk "Prev session")
    "s n" '(persp-next :wk "Next session"))

  (suzu/leader-keys
    "b" '(:ignore t :wk "buffer || bookmark")
    "b i" '(ibuffer :wk "Ibuffer")
    "b s" '(counsel-buffer-or-recentf :wk "Search buffer")
    "b k" '(kill-this-buffer :wk "Kill this buffer")
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
    "o r" '(counsel-recentf :wk "Open recent files")
    "o E" '(dired-jump :wk "Dired jump to current")
    "o e" '(projectile-dired :wk "Project root dired")
    "o p d" '(peep-dired :wk "Peep-dired")
    "o r" '(counsel-recentf :wk "Open recent files")
    "o s" '(eshell :wk "Open eshell")
    "o g" '(magit :wk "Open magit")
    "o d" '((lambda () (interactive) (flymake-show-buffer-diagnostics) (message "Buffer diagnostics") (other-window 1)) :wk "Open buffer diagnostics")
    "o D" '((lambda () (interactive) (flymake-show-project-diagnostics) (message "Project diagnostics") (other-window 1)) :wk "Open project diagnostics")
    "o t" '(multi-vterm :wk "Open Vterm")
    "o C" '((lambda () (interactive) (find-file "~/dotfiles/.emacs.d/config.org")) :wk "Edit emacs config"))

  (suzu/leader-keys
    "h" '(:ignore t :wk "Help")
    "h f" '(describe-function :wk "Describe function")
    "h v" '(describe-variable :wk "Describe variable")
    "h M" '(info-display-manual :wk "Manual")
    "h m" '(describe-mode :wk "Describe mode")
    "h r r" '((lambda ()
                (interactive)
                (load-file "~/dotfiles/.emacs.d/init.el")
                (ignore (elpaca-process-queues))) :wk "Reload emacs config"))

  (suzu/leader-keys
    "m" '(:ignore t :wk "Org")
    "m a" '(org-agenda :wk "Org agenda")
    "m e" '(org-babel-async-execute-sql :wk "Execute org babel src block")
    "m i" '(org-toggle-item :wk "Org toggle item")
    "m I" '(org-toggle-inline-images :wk "Org toggle inline images")
    "m t" '(org-todo :wk "Org todo")
    "m f" '(counsel-org-goto :wk "Find heading")
    "m B" '(org-babel-tangle :wk "Org babel tangle")
    "m l" '(org-insert-link :wk "Org insert link")
    "m T" '(org-todo-list :wk "Org todo list"))

  (suzu/leader-keys
    "m b" '(:ignore t :wk "Tables")
    "m b -" '(org-table-insert-hline :wk "Insert hline in table"))

  (suzu/leader-keys
    "m d" '(:ignore t :wk "Date/deadline")
    "m d t" '(org-time-stamp :wk "Org time stamp"))

    (suzu/leader-keys
    "c a" '(eglot-code-actions :wk "Code actions")
    "r n" '(eglot-rename :wk "Rename"))

  ;; (suzu/leader-keys
  ;;   "p" '(projectile-command-map :wk "Projectile"))

  (suzu/leader-keys
    "t" '(:ignore t :wk "Toggle")
    "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
    "t i" '(eglot-inlay-hints-mode :wk "Toggle inlay hints")
    "t c" '(suzu/center-buffer :wk "Center buffer")
    "t t" '(visual-line-mode :wk "Toggle truncated lines"))

  )

(require 'catppuccin-theme)
(load-theme 'catppuccin :no-confirm)
(setq catppuccin-flavor 'frappe) ;; or 'latte, 'macchiato, or 'mocha
(catppuccin-reload)

(use-package magit
  :ensure t
  :config
  (setq magit-status-buffer-switch-function 'switch-to-buffer)
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))



(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode +1))

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 15
        doom-modeline-buffer nil
        doom-modeline-buffer-name nil
        doom-modeline-bar-width 6
        doom-modeline-lsp t
        doom-modeline-github nil
        doom-modeline-mu4e nil
        doom-modeline-irc t
        doom-modeline-minor-modes nil
        doom-modeline-persp-name nil
        doom-modeline-display-default-persp-name nil
        doom-modeline-persp-icon nil
        doom-modeline-major-mode-icon nil))
;; (setq-default mode-line-format nil)

(defun suzu/simple-header-line-render (left right)
  "Return a string of `window-width' length containing LEFT, and RIGHT
 aligned respectively."
  (let* ((available-width (- (window-width) (length left) 1)))
    (format (format " %%s %%%ds " available-width) left right)))

(defun suzu/current-perspective ()
  (format " %s" (persp-current-name))
  )

(defun suzu/current-file-or-buffer ()
  (format " %s" (format-mode-line "%b"))
)

(setq-default header-line-format
	      '((:eval (format " %s %s"
			(suzu/current-perspective)
			(suzu/current-file-or-buffer)
			))))

(use-package all-the-icons
  :ensure t)
(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

(setq make-backup-files nil)

(use-package corfu
  :ensure t
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 1)
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect 'prompt)      ;; Preselect the prompt
  (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  ;; (setq tab-always-indent 'complete)
  )

(use-package diminish
  :ensure t)

(use-package dired-open
  :ensure t
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
  :ensure t
  :hook (evil-normalize-keymaps . peep-dired-hook)
  )

(setf dired-kill-when-opening-new-dired-buffer t)
(setq-default dired-listing-switches "-aBhl  --group-directories-first")

(use-package elfeed
  :ensure t
  :config
  (setq elfeed-search-feed-face ":foreground #ffffff :weight bold"
        elfeed-feeds (quote
                      (("https://www.reddit.com/r/linux.rss" reddit linux)
                       ("https://www.reddit.com/r/commandline.rss" reddit commandline)
                       ("https://www.reddit.com/r/distrotube.rss" reddit distrotube)
                       ("https://www.reddit.com/r/emacs.rss" reddit emacs)
                       ("https://www.gamingonlinux.com/article_rss.php" gaming linux)
                       ("https://hackaday.com/blog/feed/" hackaday linux)
                       ("https://opensource.com/feed" opensource linux)
                       ("https://linux.softpedia.com/backend.xml" softpedia linux)
                       ("https://itsfoss.com/feed/" itsfoss linux)
                       ("https://www.zdnet.com/topic/linux/rss.xml" zdnet linux)
                       ("https://www.phoronix.com/rss.php" phoronix linux)
                       ("http://feeds.feedburner.com/d0od" omgubuntu linux)
                       ("https://www.computerworld.com/index.rss" computerworld linux)
                       ("https://www.networkworld.com/category/linux/index.rss" networkworld linux)
                       ("https://www.techrepublic.com/rssfeeds/topic/open-source/" techrepublic linux)
                       ("https://betanews.com/feed" betanews linux)
                       ("http://lxer.com/module/newswire/headlines.rss" lxer linux)
                       ("https://distrowatch.com/news/dwd.xml" distrowatch linux)))))


(use-package elfeed-goodies
  :ensure t
  :init
  (elfeed-goodies/setup)
  :config
  (setq elfeed-goodies/entry-pane-size 0.5))

(set-face-attribute 'default nil
                    :font "iosevka nf"
                    :height 130
                    :weight 'medium)
(set-face-attribute 'variable-pitch nil
                    :font "Iosevka Lyte Term"
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

(setq-default line-spacing 0.12)

(setq ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

(defun suzu/ediff-hook ()
(ediff-setup-keymap)
(define-key ediff-mode-map "j" 'ediff-next-difference)
(define-key ediff-mode-map "k" 'ediff-previous-difference))

(add-hook 'ediff-mode-hook 'suzu/ediff-hook)

(use-package evil-nerd-commenter
  :ensure t
  :config
  (general-define-key
   :states 'normal
   :prefix "g"
   "c" '(evilnc-comment-or-uncomment-lines :wk "Comment lines")))

(use-package dashboard
  :ensure t
  :config
  (add-hook 'elpaca-after-init-hook #'dashboard-insert-startupify-lists)
  (add-hook 'elpaca-after-init-hook #'dashboard-initialize)
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (setq dashboard-display-icons-p t)
  (setq dashboard-path-max-length 10)
  (setq dashboard-vertically-center-content nil)
  :custom
  (dashboard-startup-banner "/home/suzu/.emacs.d/images/official.png")
  (dashboard-center-content t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
)

(use-package eldoc-box
  :ensure t
  :config
  (setq max-mini-window-height 1))

(defun suzu/rust-mode()
(add-hook 'after-save-hook 'rust-format-buffer))

(use-package rust-mode
  :ensure t
  :hook
  (rust-mode . suzu/rust-mode))

(defun suzu/python-mode()
  (add-hook 'after-save-hook 'python-black-buffer)
  (add-hook 'after-save-hook 'python-sort-imports)
  (eglot-ensure)
  (python-ts-mode))

(use-package python
  :hook
  (python-mode . suzu/python-mode))

(use-package python-black
  :ensure t)

(setq read-process-output-max (* 1024 1024))

(use-package eglot
  :config
  ;; (add-to-list 'eglot-server-programs '(python-ts-mode . ("pyright")))
  (add-to-list 'eglot-server-programs '(rust-mode . ("rust-analyzer")))
  (general-define-key
   :states '(normal visual motion)
   :keymaps 'override
   "K" '(eldoc-box-help-at-point :wk "Show doc"))
  :hook
  (rust-mode . eglot-ensure)
)

(use-package sqlformat
:ensure t
:config
(setq sqlformat-command 'pgformatter)
(setq sqlformat-args '("-s2" "-g"))
:hook
(sql-mode-hook . sqlformat-on-save-mode))

(setq-default indent-tabs-mode nil)
(electric-indent-mode t)
(setq-default electric-indent-inhibit t)
(setq create-lockfiles nil)
(electric-pair-mode 1)
(setq org-edit-src-content-indentetion 0)
(global-auto-revert-mode t)  ;; Automatically show changes if the file has changed
(menu-bar-mode -1)           ;; Disable the menu bar
(scroll-bar-mode -1)         ;; Disable the scroll bar
(tool-bar-mode -1)           ;; Disable the tool bar
(setq-default auto-save-default nil)
(setq-default org-edit-src-content-indentation 0) ;; Set src block automatic indent to 0 instead of 2
(set-fringe-style 0)

(setq-default truncate-lines t)
(setq-default scroll-margin 7)
(global-display-line-numbers-mode 1)
(setq-default display-line-numbers-type 'relative)

(setq help-window-select t)

(use-package emojify
  :ensure t
  :hook (after-init . global-emojify-mode))

(require 'org-tempo)
(add-hook 'org-mode-hook (lambda ()
			   (setq-local electric-pair-inhibit-predicate
				       `(lambda (c)
					  (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))

(setq org-confirm-babel-evaluate nil)

(setq-default plantuml-exec-mode "plantuml")

(org-babel-do-load-languages 'org-babel-load-languages
			     '((shell . t)
			       (python . t)
			       (sqlite . t)
			       (emacs-lisp . t)
			       (plantuml . t)
			       (sql . t)))

(defun suzu/visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :ensure t
  :hook
  (org-mode . suzu/visual-fill)
  (python-mode . suzu/visual-fill)
  (python-ts-mode . suzu/visual-fill)
  (rust-mode . suzu/visual-fill)
  (html-mode . suzu/visual-fill)
  (dired-mode . suzu/visual-fill))

(use-package counsel
  :ensure t
  :diminish
  :after ivy
  :config
  (use-package flx
    :ensure t)
  (counsel-mode)
  (ivy-mode 1)
  (setq ivy-re-builders-alist
	'((t . ivy--regex-plus))))

(use-package ivy
  :ensure t
  :diminish
  :bind
  ;; ivy-resume resumes the last Ivy-based completion.
  (("C-c C-r" . ivy-resume)
   ("C-x B" . ivy-switch-buffer-other-window))
  :init
  (setq ivy-initial-inputs-alist nil)
  :custom
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)
  :config
  (ivy-mode))

(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :ensure t
  :after ivy
  :init (ivy-rich-mode 1) ;; this gets us descriptions in M-x.
  :custom
  (ivy-virtual-abbreviate 'full
                          ivy-rich-switch-buffer-align-virtual-buffer t
                          ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer))

(use-package toc-org
  :ensure t
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets :ensure t)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(setq-default org-image-actual-width nil)

(use-package org-appear
  :ensure t
  :hook (org-mode-hook . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autolinks t
        org-appear-autosubmarkers t
        org-appear-autoentities t
        org-appear-trigger 'always))

(use-package org-auto-tangle
  :ensure t
  :defer t
  :hook (org-mode . org-auto-tangle-mode))

(defun suzu/org-babel-run-after-save-hook ()
    (message "Added org-babel-run-after-tangle hook")
    (add-hook 'after-save-hook (lambda () (org-babel-ref-resolve "run-after-save")))
)

;; (add-hook 'org-mode-hook 'suzu/org-babel-run-after-save-hook)

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))

(use-package projectile
  :ensure t
  :config
  (projectile-mode 1)
  (setq projectile-project-search-path '(("~/code" . 4)))
  )


(use-package persp-projectile
  :ensure t
  :after perspective)

(use-package counsel-projectile
  :ensure t)

(use-package rainbow-mode
  :ensure t
  :diminish
  :hook
  ((org-mode prog-mode) . rainbow-mode))

(use-package eshell-syntax-highlighting
  :ensure t
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode +1))

;; eshell-syntax-highlighting -- adds fish/zsh-like syntax highlighting.
;; eshell-rc-script -- your profile for eshell; like a bashrc for eshell.
;; eshell-aliases-file -- sets an aliases file for the eshell.

(setq eshell-rc-script (concat user-emacs-directory "eshell/profile")
      eshell-aliases-file (concat user-emacs-directory "eshell/aliases")
      eshell-history-size 5000
      eshell-buffer-maximum-lines 5000
      eshell-hist-ignoredups t
      eshell-scroll-to-bottom-on-input t
      eshell-destroy-buffer-when-process-dies t
      eshell-visual-commands'("bash" "fish" "htop" "ssh" "top" "zsh"))

(use-package vterm
  :ensure t
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

  :general
  (:states 'insert
           :keymaps 'vterm-mode-map
           "<tab>" 'vterm-completion))

(use-package vterm-toggle
  :ensure t
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
  :ensure t
  :config
  (add-hook 'vterm-mode-hook
            (lambda ()
              (setq-local evil-insert-state-cursor 'box)
              (evil-insert-state)))
  (define-key vterm-mode-map [return]                      #'vterm-send-return))

(use-package sudo-edit
  :ensure t
  :config
  (suzu/leader-keys
    "o w s" '(sudo-edit :wk "Sudo edit file")))

(use-package tldr :ensure t)

(add-to-list 'default-frame-alist '(alpha-background . 100))

(setq treesit-language-source-alist
      '((rust "https://github.com/tree-sitter/tree-sitter-rust")
        (python "https://github.com/tree-sitter/tree-sitter-python")))

(setq treesit-font-lock-level 4)

(use-package which-key
  :ensure t
  :diminish
  :init
  (which-key-mode)
  :config
  (setq which-key-popup-type 'side-window
        which-key-side-window-max-height 0.50))

(use-package perspective
  :ensure t
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

(add-hook 'kill-emacs-hook #'persp-state-save)

(use-package bufler
  :ensure t)
