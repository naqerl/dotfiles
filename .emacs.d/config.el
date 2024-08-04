(add-to-list 'load-path "~/dotfiles/.emacs.d/scripts")
(add-to-list 'load-path "~/dotfiles/.emacs.d/themes")
(require 'elpaca-setup)

(use-package gcmh
  :ensure t
  :init
  (gcmh-mode 1))

(setq-default debug-on-error nil)

(require 'pyright-write)
(require 'suzu-buffer)
(require 'suzu-project)

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
(setq org-return-follows-link t)

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

(with-eval-after-load 'evil-maps
  (global-set-key (kbd "C-h") nil)
  (global-set-key (kbd "C-k") nil)
  (global-unset-key (kbd "C-j"))
  (global-unset-key (kbd "C-l"))
  (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
)

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
    "." '(find-file :wk "Find file")
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

;; (require 'catppuccin-theme)
;; (load-theme 'catppuccin :no-confirm)
;; (setq catppuccin-flavor 'frappe) ;; or 'latte, 'macchiato, or 'mocha
;; (catppuccin-reload)

(setq modus-themes-mode-line '(borderless 3d)
      modus-themes-region '(bg-only)
      modus-themes-org-blocks 'gray-background
      modus-themes-completions '((selection intense) (popup intense))
      modus-themes-bold-constructs t
      modus-themes-italic-constructs t
      modus-themes-paren-match '(bold)
      modus-themes-syntax '(green-strings yellow-comments)
      modus-themes-headings
      '((1 . (rainbow 1.5))
        (2 . (rainbow 1.3))
        (3 . (rainbow bold 1.2))
        (t . (semilight 1.1 ))))
(load-theme 'modus-vivendi :no-confirm)

(setq-default display-line-numbers-width 4)

(use-package auto-dim-other-buffers
  :disabled
  :ensure t
  :custom
  (auto-dim-other-buffers-dim-on-switch-to-minibuffer nil)
  (auto-dim-other-buffers-affected-faces '((default . auto-dim-other-buffers-face)
                                           (org-hide . auto-dim-other-buffers-hide-face))))

(use-package magit
  :ensure t
  :config
  (setq magit-status-buffer-switch-function 'switch-to-buffer)
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))

(use-package git-gutter
  :ensure t
  :custom
  (git-gutter:modified-sign " ") ;; two space
  (git-gutter:added-sign " ")    ;; multiple character is OK
  (git-gutter:deleted-sign " ")
  :config
  (global-git-gutter-mode +1))

;; (use-package doom-modeline
;;   :ensure t
;;   :init
;;   (doom-modeline-mode nil)
;;   :config
;;   (setq doom-modeline-height 15
;;         doom-modeline-buffer nil
;;         doom-modeline-buffer-name nil
;;         doom-modeline-bar-width 6
;;         doom-modeline-lsp t
;;         doom-modeline-github nil
;;         doom-modeline-mu4e nil
;;         doom-modeline-irc t
;;         doom-modeline-minor-modes nil
;;         doom-modeline-persp-name nil
;;         doom-modeline-display-default-persp-name nil
;;         doom-modeline-persp-icon nil
;;         doom-modeline-major-mode-icon nil))
(setq-default mode-line-format nil)

(use-package all-the-icons
  :ensure t)
(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

(setq make-backup-files nil)

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-delay 1)
  (corfu-auto-prefix 2)
  ;; (corfu-echo-documentation 0.25)
  ;; ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :bind (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  ;; (define-key corfu-map (kbd "M-j") #'corfu-doc-scroll-down)
  ;; (define-key corfu-map (kbd "M-k") #'corfu-doc-scroll-up)
  :init
  ;; (add-hook 'corfu-mode-hook #'corfu-popupinfo-mode)
  (global-corfu-mode))

;; (defun corfu-send-shell (&rest _)
;;   "Send completion candidate when inside comint/eshell."
;;   (cond
;;    ((and (derived-mode-p 'eshell-mode) (fboundp 'eshell-send-input))
;;     (eshell-send-input))
;;    ((and (derived-mode-p 'comint-mode)  (fboundp 'comint-send-input))
;;     (comint-send-input))))

;; (advice-add #'corfu-insert :after #'corfu-send-shell)

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
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package nerd-icons-corfu
:ensure t
:config
(add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

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
                       ("https://www.reddit.com/r/unixport.rss" reddit)
                       ("https://www.reddit.com/r/emacsporn.rss" reddit)
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
                       ("https://systemcrafters.net/rss/news.xml" emac)
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
   "K" '(eldoc-box-help-at-point :wk "Show doc")
   ;; "C-k" '(suzu/eldoc-box-scroll-up)
   ;; "C-j" '(suzu/eldoc-box-scroll-down)
   )
  ;; :general
  ;; (:keymaps 'eglot-mode-map
  ;;           "C-k" 'rex/eldoc-box-scroll-up
  ;;           "C-j" 'rex/eldoc-box-scroll-down
  ;;           "K" 'eldoc-box-eglot-help-at-point)
  )

;; (use-package eldoc-box
;;   :ensure t
;;   :config
;;   (general-define-key
;;    :states '(normal visual motion)
;;    :keymaps 'override
;;    "K" '(eldoc-box-help-at-point :wk "Show doc")))

(defun suzu/rust-mode()
  ;; (add-hook 'after-save-hook 'rust-format-buffer)
  (eglot-ensure))

(use-package rust-mode
  :ensure t
  :init
  (setq rust-mode-treesitter-derive t)
  (setq rust-format-on-save t))
(add-hook 'rust-mode-hook 'suzu/rust-mode)

(defun suzu/python-mode()
  (add-hook 'before-save-hook 'python-black-buffer)
  (add-hook 'before-save-hook 'python-sort-imports)
  (eglot-ensure))

(use-package python
  :hook
  (python-ts-mode . suzu/python-mode))

(use-package python-black
  :ensure t)

(use-package yuck-mode
  :ensure t)

(use-package sqlformat
:ensure t
:config
(setq sqlformat-command 'pgformatter)
(setq sqlformat-args '("-s2" "-g"))
:hook
(sql-mode-hook . sqlformat-on-save-mode))

(use-package markdown-mode
  :ensure t)

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
  :ensure t)

(require 'org-tempo)
(add-hook 'org-mode-hook
  (lambda ()
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
  :config
  :hook
  (org-mode . suzu/visual-fill)
  (dired-mode . suzu/visual-fill)
  (eshell-mode . suzu/visual-fill)
  (prog-mode . suzu/visual-fill)
  (text-mode . suzu/visual-fill))

(use-package toc-org
  :ensure t
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets
 :ensure t
 :hook (org-mode . org-bullets-mode)
 :custom (org-bullets-bullet-list '("◉" "○" "󰣏" "󱀝" "󰴈" "○" "●")))

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

(defun suzu/run-after-tangle-hook ()
    (add-hook 'org-bable-tangle-finished-hook (lambda () (org-babel-ref-resolve "run-after-save")))
)

;; (add-hook 'org-mode-hook 'suzu/org-babel-run-after-save-hook)

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
				  (":end:" . "―")
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
				  ("scheduled:" . "")
				  ("deadline:" . "")))
   (prettify-symbols-mode))
(add-hook 'org-mode-hook 'suzu/org-icons)

(defun suzu/setup-org-mode ()
    (evil-define-key '(normal) org-mode-map (kbd "C-k") 'evil-window-up)
    (evil-define-key '(normal) org-mode-map (kbd "C-j") 'evil-window-down))

(add-hook 'org-mode-hook 'suzu/setup-org-mode)

;; (defun suzu/disable-git-gutter ()
;;   (git-gutter-mode nil))
;; (add-hook 'org-mode-hook 'suzu/disable-git-gutter)

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))

(use-package marginalia
  :ensure t
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'left)
  :init
  (marginalia-mode))

(use-package all-the-icons-completion
  :ensure t
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))

(use-package vertico
  :ensure t
  :custom
  (vertico-count 13)
  (vertico-resize nil)
  (vertico-cycle nil)
  :config
  (vertico-mode))

(use-package consult
  :ensure t
  :config
)

(use-package rainbow-mode
  :ensure t
  :diminish
  :hook
  ((org-mode prog-mode) . rainbow-mode))

(setq comint-input-ignoredups t)

(use-package eshell-syntax-highlighting
  :ensure t
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode +1))

(use-package eshell-git-prompt
  :ensure t)

(defun suzu/eshell-prompt ()
  "Eshell prompt"
  (let (separator hr dir git git-dirty time sign command)
    (setq separator (with-face " | " 'eshell-git-prompt-multiline-secondary-face))
    (setq dir
          (concat
           (with-face "  " 'eshell-git-prompt-directory-face)
           (concat  (abbreviate-file-name (eshell/pwd)))))
    (setq time (with-face (format-time-string "%I:%M:%S %p") 'eshell-git-prompt-multiline-secondary-face))
    (setq sign
          (if (= (user-uid) 0)
              (with-face "\n#" 'eshell-git-prompt-multiline-sign-face)
            (with-face "\nλ" 'eshell-git-prompt-multiline-sign-face)))
    (setq command (with-face " " 'eshell-git-prompt-multiline-command-face))

    ;; Build prompt
    (eshell-git-prompt---str-read-only
     (concat dir separator time sign command))))

(defconst suzu/eshell-prompt-regexp "^[^$\n]*λ ")

(defun suzu/configure-eshell ()
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'consult-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-k") 'evil-window-up)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-j") 'evil-window-down)
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
        eshell-prompt-function 'suzu/eshell-prompt
        eshell-prompt-regexp suzu/eshell-prompt-regexp
        eshell-visual-commands '("bash" "fish" "htop" "ssh" "top" "zsh")))

(use-package eshell-toggle
  :ensure t
  :custom
  (eshell-toggle-window-side 'above)
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-projectile-root nil)
  (eshell-toggle-run-command nil))

(use-package pcmpl-args
  :ensure t)

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

  ;; :general
  ;; (:states 'insert
  ;;          :keymaps 'vterm-mode-map
  ;;          "<tab>" 'vterm-completion)
)

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
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp")))

(setq treesit-font-lock-level 4)
(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)
        (rust-ts-mode . rust-mode)))

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
  (if (buffer-modified-p)
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

(use-package org-ai
  :ensure t
  :commands (org-ai-mode
             org-ai-global-mode)
  :init
  (add-hook 'org-mode-hook #'org-ai-mode) ; enable org-ai in org-mode
  (org-ai-global-mode) ; installs global keybindings on C-c M-a
  :config
  (setq org-ai-default-chat-model "gpt-3.5") ; if you are on the gpt-4 beta:
) ; if you are using yasnippet and want `ai` snippets

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
   :token "xoxc-4777443326-4451074101014-6449473241250-0554c5b458a7399d1813d75f1cbc04026038a40db5e26c275ec8d50e0fcd9194"
   :cookie "xoxd-wuTr0YHy50a34f9zV6PmVqE7AOaMkf%2BydScfh8chKzySac8nlsZi%2BdltbY2XJMM%2FNKfl3FdJRNq0dTH2mGAgTMHJ1ZQV5GBDz9TbbSQ440WJOqyriJXRrdhJH2o0GlVUa5Yb2BiULElCQ0BSRpNaYqDk%2FjQfUUbyKLWqOBvW88wutWI%2FJgUi6ELKXVaSgsZ9wYvDDhE%3D"))

(use-package alert
  :ensure t
  :commands (alert)
  :init
  (setq alert-default-style 'libnotify))
