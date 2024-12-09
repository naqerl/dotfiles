(provide 'sp-build)

(cl-defstruct sp/makefile-target name comment prerequisites)

(defun sp/parse-makefile ()
  "Assumes current buffer is makefile.
Parses it with tree-sitter and returns alist of (target . comment)"
  (let* ((parser (treesit-parser-create 'make))
         (root (treesit-buffer-root-node))
         (target-nodes
          (treesit-query-capture
           root
           '(((comment)
              :? @comment
              :anchor
              (rule
               (targets (word) @target)
               normal:
               (prerequisites (word))
               :? @prerequisites)))))
         (targets
          (mapcar
           (lambda (el)
             (cons (car el) (treesit-node-text (cdr el))))
           target-nodes))
         (result '()))
    (sp/format-treesit-nodes targets)))

(defun sp/format-treesit-nodes (nodes)
  "Format a list of treesit nodes into an alist of (target . 'prerequisites, comment)."
  (let ((result '())
        (current-target nil)
        (current-prerequisites nil)
        (current-comment nil))
    (dolist (node nodes)
      (let ((key (car node))
            (value (cdr node)))
        (message "Processing key=%s with value=%s" key value)
        (cond
         ((eq key 'comment)
          (when current-target
            (push (cons
                   current-target
                   (make-sp/makefile-target
                    :name current-target
                    :comment current-comment
                    :prerequisites current-prerequisites))
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
            (push (cons
                   current-target
                   (make-sp/makefile-target
                    :name current-target
                    :comment current-comment
                    :prerequisites current-prerequisites))
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
      (push (cons
             current-target
             (make-sp/makefile-target
              :name current-target
              :comment current-comment
              :prerequisites current-prerequisites))
            result))
    result))
