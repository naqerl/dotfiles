(provide 'sp-build)

(cl-defstruct
 sp--makefile-target
 "Represents Makefile target and it's context"
 (name :read-only)
 (comment :read-only)
 (prerequisites :read-only))

(defvar sp-column-margin 4
  "Amount of spaces during `completing-read' between Makefile target name, prerequisites and comment")

(defvar sp--treesit-qeury
  '(((comment)
     :? @comment
     :anchor
     (rule
      (targets (word) @target)
      normal:
      (prerequisites (word))
      :? @prerequisites)))
  "Tree-sitter query to construct `sp--makefile-target' from")

(defun sp-makefile-compile ()
  (interactive)
  (let*
      ((default-directory (project-root (project-current)))
       (targets (sp--parse-project-makefile))
       (max-target-width
        (sp--max-width-by 'sp--makefile-target-name targets))
       (max-prerequisites-width
        (sp--max-width-by 'sp--makefile-target-prerequisites targets))
       (target-column-width (+ max-target-width sp-column-margin))
       (prerequisites-column-width
        (+ max-prerequisites-width sp-column-margin))
       (targets-alist
        (mapcar
         (lambda (el)
           (cons (sp--makefile-target-name el) el))
         targets))
       (completion-extra-properties ;; Create padded prerequisites and comments annotation
        '(:annotation-function
          (lambda (target-name)
            (let* ((target
                    (alist-get target-name minibuffer-completion-table
                               nil nil #'string=))
                   (prerequisites
                    (sp--makefile-target-prerequisites target))
                   (comment (sp--makefile-target-comment target))
                   (target-column-padding
                    (sp--calculate-padding target-column-width
                                           target-name))
                   (prerequisites-column-padding
                    (sp--calculate-padding prerequisites-column-width
                                           prerequisites)))
              (s-concat
               (make-string target-column-padding ?\s)
               (propertize (if prerequisites
                               prerequisites
                             "")
                           'face 'package-status-dependency)
               (make-string prerequisites-column-padding ?\s)
               (propertize (if comment
                               comment
                             "")
                           'face 'completions-annotations))))))
       (target (completing-read "Make target: " targets-alist)))
    (compile (format "make %s" target))))

(defun sp--parse-project-makefile ()
  "Search for the Makefile in the `default-directory' and return a table of available targets."
  (let ((makefile (expand-file-name "Makefile")))
    (if (file-exists-p makefile)
        (with-temp-buffer
          (insert-file-contents makefile)
          (sp--parse-makefile))
      (warn "No Makefile found in project root"))))

(defun sp--parse-makefile ()
  "Assumes that Makefile opened in the current buffer
   Parses it with tree-sitter and returns a `list' of `sp--makefile-target'"
  (let* ((parser (treesit-parser-create 'make))
         (root (treesit-buffer-root-node))
         (target-nodes (treesit-query-capture root sp--treesit-qeury))
         (targets
          (mapcar
           (lambda (el)
             (cons (car el) (treesit-node-text (cdr el))))
           target-nodes)))
    (sp--format-treesit-nodes targets)))

(defun sp--format-treesit-nodes (nodes)
  "Format a list of treesit nodes into a list of `sp--makefile-target'"
  (let ((result '())
        (current-target nil)
        (current-prerequisites nil)
        (current-comment nil))
    (dolist (node nodes)
      (let ((key (car node))
            (value (cdr node)))
        (cond
         ((eq key 'comment)
          (when current-target
            (push (make-sp--makefile-target
                   :name current-target
                   :comment current-comment
                   :prerequisites current-prerequisites)
                  result)
            (setq
             current-target nil
             current-prerequisites nil))
          (setq current-comment value))
         ((eq key 'prerequisites)
          (when current-target
            (setq current-prerequisites value)))
         ((eq key 'target)
          (when current-target
            (push (make-sp--makefile-target
                   :name current-target
                   :comment current-comment
                   :prerequisites current-prerequisites)
                  result)
            (setq
             current-prerequisites nil
             current-comment nil))
          (if (string= (downcase value) ".phony")
              (setq current-target nil)
            (setq current-target value)))
         (t
          (warn
           "ERROR: Unknown makefile treesit node name=%s, value=%s"
           key
           value)))))
    (when current-target
      (push (make-sp--makefile-target
             :name current-target
             :comment current-comment
             :prerequisites current-prerequisites)
            result))
    result))

(defun sp--max-width-by (predicate sequence)
  "Max width of string returned by `predicate' applied to each element from `sequence'"
  (-reduce
   #'max
   (mapcar
    (lambda (el)
      (let ((value (funcall predicate el)))
        (if value
            (string-width value)
          0)))
    sequence)))

(defun sp--calculate-padding (max-width &optional value)
  "Diff of `max-widt' and `value'"
  (- max-width
     (if value
         (string-width value)
       0)))
