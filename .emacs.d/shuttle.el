;;; package --- LSP setup ;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; lsp-mode & corfu setup to get lsp features
;;; Code:
(setq read-process-output-max (* 10 1024 1024))
(setq gc-cons-threshold 200000000)

(use-package lsp-mode
  :ensure t
  :preface
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :custom
  (lsp-completion-provider :none)       ; Using Corfu as the provider
  (lsp-diagnostics-provider :flycheck)
  :hook
  (tsx-mode . lsp)
  :commands lsp)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error) ; optional but recommended error navigation
              ("M-p" . flycheck-previous-error)))


(use-package lsp-tailwindcss
  :load-path "~/.emacs.d/lsp-tailwindcss"
  :init (setq lsp-tailwindcss-add-on-mode t)
  :config
  (dolist (tw-major-mode
           '(css-mode
             css-ts-mode
             typescript-mode
             typescript-ts-mode
             tsx-ts-mode
             tsx-mode
             js2-mode
             js-ts-mode
             jtsx-tsx-mode
             clojure-mode))
    (add-to-list 'lsp-tailwindcss-major-modes tw-major-mode)))

(use-package lsp-ui :ensure t :commands lsp-ui-mode)

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  (setq corfu-auto t
      corfu-auto-delay 0
      corfu-auto-prefix 0))

(use-package emacs
  :custom
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil)
  (read-extended-command-predicate #'command-completion-default-include-p))

(provide 'shuttle)
;;; shuttle.el ends here
