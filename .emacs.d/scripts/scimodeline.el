(defface my-modeline-background
  '((t :background "#5f509f" :foreground "white" :inherit bold))
  "Face with a accent background for use on the mode line.")

(defface my-modeline-alert-bg
  '((t :background "#b52c2c" :foreground "white" :inherit bold))
  "Face with a red background for use on the mode line.")

(defface my-modeline-warn-fg '((t :foreground "orange" :inherit bold))
  "Face to show warn messages.")

(defface my-modeline-accent-fg '((t :foreground "#2fafff"))
  "Accent face")

(defun my-modeline--buffer-name ()
  "Return `buffer-name' with spaces around it."
  (format " %s " (buffer-name)))

(defvar-local my-modeline-buffer-name
    '(:eval
      (when (mode-line-window-selected-p)
        (concat

         (propertize (my-modeline--buffer-name)
                     'face
                     'my-modeline-background))))
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
         (propertize "λ" 'face 'shadow)
         " "
         (propertize (my-modeline--major-mode-name) 'face 'bold)))
      "Mode line construct to display the major mode."))

(put 'my-modeline-major-mode 'risky-local-variable t)

(defvar-local my-modeline-timer
    '(:eval
      (when (and (boundp 'org-timer-mode-line-string)
                 (mode-line-window-selected-p))
        (let* ((time
                (replace-regexp-in-string
                 "[\<\>]" "" org-timer-mode-line-string))
               (status
                (if (string= time " 0:00:01")
                    (propertize " TIMER DONE "
                                'face
                                'my-modeline-alert-bg)
                  (propertize time 'face 'bold))))
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

(defvar-local my-persp-name
    '(:eval
      (when (mode-line-window-selected-p)
        (propertize (persp-current-name) 'face 'bold)))
  "Mode line construct to display current perspective name.")
(put 'my-persp-name 'risky-local-variable t)

(defvar-local my-compilation-in-progress
    '(:eval
      (when (and (boundp 'compilation-in-progress)
                 compilation-in-progress
                 (mode-line-window-selected-p))
        (propertize "󱁤  " 'face 'my-modeline-warn-fg)))
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

(setq-default mode-line-format nil)

(defface scimodeline-hide-face
  `((t
     :overline "#BEC3C6"
     :height 1
     :font "Iosevka NF 7"
     :box nil
     :background ,(plist-get grayscale-theme-colors :bg)))
  "Face with a accent background for use on the mode line.")


;; Declared as var cause :line-width (1 . 0) is forbidden
(defvar scimodeline-header-outline
  '(:overline
    "#BEC3C6"
    :underline nil
    :box (:color "#BEC3C6" :line-width (1 . 0)))
  "Doc string.")

(defun sci-box-buffer ()
  "Doc string."
  (interactive)
  (setq
   mode-line-format
   `((:eval ,(propertize " " 'face 'scimodeline-hide-face)))
   fringes-outside-margins t
   left-margin-width 0
   right-margin-width 0
   left-fringe-width 1
   right-fringe-width 1)
  (face-remap-add-relative 'mode-line-active 'scimodeline-hide-face)
  (face-remap-add-relative 'mode-line-inactive 'scimodeline-hide-face)
  (face-remap-add-relative 'header-line scimodeline-header-outline)
  (face-remap-add-relative
   'header-line-active scimodeline-header-outline)
  (face-remap-add-relative
   'header-line-inactive scimodeline-header-outline)
  (set-window-margins nil 1 1)
  (when (eq (window-buffer) (current-buffer))
    (set-window-buffer nil (current-buffer))))

(setq window-divider-default-right-width 10)
(window-divider-mode t)

(defun my/display-buffer-box (window)
  (with-current-buffer (window-buffer window)
    (sci-box-buffer)))
(add-to-list
 'display-buffer-alist
 '(".*" (display-buffer-reuse-window
    display-buffer-same-window)
   (body-function . my/display-buffer-box)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

 '(fringe ((t (:foreground "#BEC3C6" :background "#BEC3C6"))))
 `(window-divider
   ((t (:foreground ,(plist-get grayscale-theme-colors :bg)))))
 `(window-divider-first-pixel
   ((t (:foreground ,(plist-get grayscale-theme-colors :bg)))))
 `(window-divider-last-pixel
   ((t (:foreground ,(plist-get grayscale-theme-colors :bg))))))


(setq-default header-line-format
              '("" my-modeline-buffer-name "  " my-modeline-major-mode
                ;; mode-line-format-right-align
                ;; my-persp-name
                ;; " "
                ;; my-compilation-in-progress
                ;; my-modeline-lsp
                ;; my-modeline-timer
                ))

(defun scimodeline-setup-minibuffer ()
  "Removes fringes for the minibuffer."
  (message "Entering mini buffer")
  (set-window-fringes (minibuffer-window) 0))
(add-hook 'minibuffer-mode-hook 'scimodeline-setup-minibuffer)


(provide 'scimodeline)
