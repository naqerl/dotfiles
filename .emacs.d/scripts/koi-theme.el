;;; koi-theme.el --- An elegant dark theme -*- lexical-binding: t; -*-

;;; Commentary:

;; Dark theme inspired by Kanagawa and grayscale themes

;;; Code:
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

(defconst koi-theme-colors
  `(:fg-2 "#393836"
    :fg-1 "#717C7C"
    :fg "#c5c9c5"
    :fg+1 "#a6a69c"
    :bg-1 "#0d0c0c"
    :bg "#181616"
    :bg+1 "#282727"
    :aqua "#8ea4a2"
    :aqua+1 "#597b75"
    :green "#87a987"
    :blue "#7E9CD8"
    :yellow "#c4b28a"
    :red "#C34043"
    :orange "#FF9E3B"))

(koi-theme-set-faces
 'koi
 koi-theme-colors
 '(
   (default                                     :background bg		:foreground fg)
   (cursor                                      :background green	:foreground bg		:weight bold)
   (region                                      :background fg-2)
   (line-number                                 :background bg		:foreground fg-2)
   (line-number-current-line                    :background bg		:foreground green	:weight bold)
   (mode-line                                   :background bg-1)
   (mode-line-inactive                          :background bg-1	:foreground fg-2	:box nil)
   (mode-line-emphasis                          :background bg+1				:box (:line-width (1 . 1) :color fg-1 :style released-button))
   (vertical-border                                                  	:foreground bg-1)
   (minibuffer-prompt                                                	:foreground aqua)
   (highlight                                   :background bg+1)
   (fringe					:background bg-1	:foreground bg-1)
   (window-divider				:background bg-1	:foreground bg-1)
   (window-divider-first-pixel			:background bg-1	:foreground bg-1)
   (window-divider-last-pixel			:background bg-1	:foreground bg-1)
   (link                                    	:foreground aqua :underline t)
   (match					:background fg-1	:foreground fg)
   (flymake-note-echo							:foreground fg)
   (xref-info					:foreground aqua)
   (success								:foreground green)

   (font-lock-builtin-face                      :foreground fg+1)
   (font-lock-comment-delimiter-face            :foreground fg-1)
   (font-lock-comment-face                      :foreground fg-1)
   (font-lock-constant-face                     :foreground fg-1)
   (font-lock-doc-face                          :foreground fg-1)
   (font-lock-doc-string-face                   :foreground fg-1)
   (font-lock-function-name-face                :foreground fg+1)
   (font-lock-keyword-face                      :foreground fg+1	:weight bold :italic t)
   (font-lock-negation-char-face                :foreground fg-1)
   (font-lock-preprocessor-face                 :foreground fg-1)
   (font-lock-regexp-grouping-backslash         :foreground fg-1)
   (font-lock-regexp-grouping-construct         :foreground fg)
   (font-lock-string-face                       :foreground aqua)
   (font-lock-type-face                         :foreground fg)
   (font-lock-variable-name-face                :foreground fg+1)
   (font-lock-warning-face                      :foreground yellow)

   (show-paren-match                            :background red)
   (show-paren-mismatch                         :background orange)

   (org-date                                    :foreground aqua)
   (org-block					:background bg+1)
   (org-block-begin-line			:background bg+1	:foreground aqua :extend t)
   (org-block-end-line				:background bg+1 	:foreground aqua :extend t)
   (org-code					:background bg+1	:foreground fg+1 :box (:line-width (1 . 1) :color fg-1 :style released-button))
   (org-ellipsis							:foreground aqua)

   (orderless-match-face-0						:foreground green)
   (orderless-match-face-1						:foreground blue)
   (orderless-match-face-2						:foreground red)
   (git-gutter:modified                          :foreground yellow)
   (git-gutter:added                          :foreground green)
   (git-gutter:deleted                          :foreground red)
  ))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'koi)
;;; koi-theme.el ends here
