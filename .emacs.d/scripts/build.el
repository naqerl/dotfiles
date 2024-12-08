(provide 'build)

(defun sp/parse-makefile ()
  "Assumes current buffer is makefile.
Parses it with tree-sitter and returns alist of (target . comment)"
  (let* ((parser (treesit-parser-create 'make))
         (root (treesit-buffer-root-node))
         (targets (treesit-node-children root))
         (result '()))
    (dolist (node targets)
      (if (treesit-node-match-p node "rule")
          (let* ((target
                  (treesit-node-get node '((child 0 nil) (text nil))))
                 (prev (treesit-node-get node '((sibling -1 nil))))
                 (comment
                  (if (and prev (treesit-node-match-p prev "comment"))
                      (treesit-node-text prev)
                    "")))
            (if (not (string= (downcase target) ".phony"))
                (setq result (cons (cons target comment) result))
              (message "Got target name [%s]" target)))
        (message "Unexpected node: %s"
                 (treesit-node-get node '((type))))))
    result))
