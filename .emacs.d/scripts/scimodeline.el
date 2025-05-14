(require 'org)
(require 'all-the-icons)

(defface my-modeline-alert-bg
  '((t :background "#b52c2c" :foreground "white" :inherit bold))
  "Face with a red background for use on the mode line.")

(defface my-modeline-warn-fg '((t :foreground "orange" :inherit bold))
  "Face to show warn messages.")

(defface my-modeline-accent-fg '((t :foreground "#2fafff"))
  "Accent face.")

(defun my-modeline--buffer-name ()
  "Return `buffer-name' with spaces around it."
  (format " %s " (buffer-name)))

(defvar-local my-modeline-buffer-name
    '(:eval
      (concat
       (propertize (my-modeline--buffer-name)
                   'face
                   (if (mode-line-window-selected-p)
                       'mode-line-emphasis
                     'mode-line-inactive))))
  "Mode line construct to display the buffer name.")

(put 'my-modeline-buffer-name 'risky-local-variable t)

(defun my-modeline--major-mode-name ()
  "Return capitalized `major-mode' as a string."
  (when (mode-line-window-selected-p)
    (capitalize (symbol-name major-mode))))

(defvar-local my-modeline-major-mode
    '(:eval
      (when (stringp (my-modeline--major-mode-name))
        (list
         (or (and (buffer-file-name)
                  (all-the-icons-icon-for-file
                   (buffer-file-name)
                   :height 0.7
                   :v-adjust 0.02))
             "")
         " " (propertize (my-modeline--major-mode-name) 'face 'bold)))
      "Mode line construct to display the major mode."))

(put 'my-modeline-major-mode 'risky-local-variable t)

(defvar-local my-modeline-timer
    '(:eval
      (when (and (boundp 'org-timer-mode-line-string)
                 (mode-line-window-selected-p))
        (let* ((time
                (string-trim
                 (replace-regexp-in-string
                  "[\<\>]" "" org-timer-mode-line-string)))
               (status
                (if (string= time "0:00:01")
                    (concat
                     (propertize "| " 'face 'mode-line)
                     (propertize "  TIMER DONE "
                                 'face
                                 'my-modeline-alert-bg))
                  (propertize (format "|  %s" time) 'face 'bold))))
          status)))
  "Mode line construct to display org timer.")
(put 'my-modeline-timer 'risky-local-variable t)

(defvar-local my-modeline-lsp
    '(:eval
      (when (and (boundp 'eglot--managed-mode)
                 eglot--managed-mode
                 (mode-line-window-selected-p))
        (propertize "  " 'face 'my-modeline-accent-fg)))
  "Mode line construct to display LSP active status.")
(put 'my-modeline-lsp 'risky-local-variable t)

(defvar-local my-compilation-in-progress
    '(:eval
      (when (and (boundp 'compilation-in-progress)
                 compilation-in-progress
                 (mode-line-window-selected-p))
        (concat
         (propertize "| " 'face 'mode-line)
         (propertize "󱁤  " 'face 'my-modeline-warn-fg))))
  "Mode line construct to display compilation process.")
(put 'my-compilation-in-progress 'risky-local-variable t)


;; Emacs 29, check the definition right below
(mode-line-window-selected-p)

(defun mode-line-window-selected-p ()
  "Return non-nil if we're updating the mode line for the selected window.
This function is meant to be called in `:eval' mode line
constructs to allow altering the look of the mode line depending
on whether the mode line s to the currently selected window
or not."
  (let ((window (selected-window)))
    (or (eq window (old-selected-window))
        (and (minibuffer-window-active-p (minibuffer-window))
             (with-selected-window (minibuffer-window)
               (eq window (minibuffer-selected-window)))))))

(setq-default mode-line-format
              '(""
                my-modeline-buffer-name
                "  "
                my-modeline-major-mode
                " "
                my-compilation-in-progress
                my-modeline-timer))


(provide 'scimodeline)
