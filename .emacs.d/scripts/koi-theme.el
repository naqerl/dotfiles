;;; koi-theme.el --- An elegant dark theme -*- lexical-binding: t; -*-

;;; Commentary:

;; Dark theme inspired by Kanagawa and grayscale themes

;;; Code:

(unless (>= emacs-major-version 24)
  (error "Requires Emacs 24 or later"))

(defgroup koi-theme
  nil
  "Koi theme options."
  :group 'faces)

(deftheme koi
  "An elegant minimal darkish theme.")



(defun koi-theme-set-faces (theme-name colors faces)
  "Define the important part of `THEME-NAME' using `COLORS' to map the `FACES' to actual colors."
  (apply 'custom-theme-set-faces theme-name
         (mapcar #'(lambda (face)
                     (koi-theme-transform-face face colors))
                 faces)))

(defun koi-theme-transform-face (spec colors)
  "Transform a face `SPEC' into an Emacs theme face definition using `COLORS'."
  (let* ((face             (car spec))
         (definition       (cdr spec)))
    (list face `((t ,(koi-theme-transform-spec definition colors))))))

(defun koi-theme-transform-spec (spec colors)
  "Transform a theme `SPEC' into a face spec using `COLORS'."
  (let ((output))
    (while spec
      (let* ((key       (car  spec))
             (value     (cadr spec))
             (color-key (if (symbolp value) (intern (concat ":" (symbol-name value))) nil))
             (color     (plist-get colors color-key)))
        ;; Append the transformed element
        (cond
         ((and (memq key '(:box :underline)) (listp value))
          (setq output (append output (list key (koi-theme-transform-spec value colors)))))
         (color
          (setq output (append output (list key color))))
         (t
          (setq output (append output (list key value))))))
      ;; Go to the next element in the list
      (setq spec (cddr spec)))
    ;; Return the transformed spec
    output))

(defconst koi-dark-colors
  `(:fg-1 "#717C7C"
    :fg "#c5c9c5"
    :fg+1 "#a6a69c"
    :bg-1 "#0d0c0c"
    :bg "#181616"
    :bg+1 "#282727"
    :lavender "#9CABCA"
    :aqua "#8ea4a2"
    :green "#87a987"
    :blue "#7E9CD8"
    :yellow "#c4b28a"
    :red "#C34043"
    :orange "#FF9E3B"))


(koi-theme-set-faces
 'koi
 koi-dark-colors
 '(
   (default                                     :background bg       :foreground fg                   )
   (cursor                                      :background green    :foreground bg       :weight bold)
   (region                                      :background bg+1                                      )
   (line-number                                 :background bg-1     :foreground fg+1                 )
   (line-number-current-line                    :background bg-1     :foreground green    :weight bold)
   (mode-line                                   :background bg-1                                      )
   (mode-line-inactive                          :background bg-1                          :box nil    )
   (vertical-border                                                  :foreground bg-1                 )
   (minibuffer-prompt                                                :foreground aqua                 )
   (highlight                                   :background bg+1                                      )

   ; begin-region -- Font lock

   (font-lock-builtin-face                      :foreground fg+1)
   (font-lock-comment-delimiter-face            :foreground fg-1)
   (font-lock-comment-face                      :foreground fg-1)
   (font-lock-constant-face                     :foreground fg-1)
   (font-lock-doc-face                          :foreground fg-1)
   (font-lock-doc-string-face                   :foreground fg-1)
   (font-lock-function-name-face                :foreground fg+1)
   (font-lock-keyword-face                      :foreground fg+1)
   (font-lock-negation-char-face                :foreground fg-1)
   (font-lock-preprocessor-face                 :foreground fg-1)
   (font-lock-regexp-grouping-backslash         :foreground fg-1)
   (font-lock-regexp-grouping-construct         :foreground fg)
   (font-lock-string-face                       :foreground fg-1)
   (font-lock-type-face                         :foreground fg)
   (font-lock-variable-name-face                :foreground fg+1)
   (font-lock-warning-face                      :foreground yellow)

   ; end-region   -- Font lock

   ; begin-region -- Show paren mode

   (show-paren-match                            :background red)
   (show-paren-mismatch                        :background orange)

   ; end-region   -- Show paren mode
  ))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'koi)
;;; koi-theme.el ends here
