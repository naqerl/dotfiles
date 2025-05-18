;;; make-project.el --- Run Makefile commands with ease -*- lexical-binding: t;-*-

;; Copyright (C) 2024 SciPunch <scipunch@gmail.com>

;; Author: SciPunch <scipunch@gmail.com>
;; Maintainer: SciPunch <scipunch@gmail.com>
;; URL: https://github.com/scipunch/make-project
;; Version: 0.1
;; Keywords: project compile make makefile

;; This is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; GNU make is a powerful tool as GNU Emacs compile mode.
;; This package helps to merge them into a powerful interface
;;

;;; Code:


(require 'project)
(require 'treesit)

(cl-defstruct
 make-project--makefile-target
 "Represents Makefile target and it's context."
 (name :read-only)
 (comment :read-only)
 (prerequisites :read-only))

(cl-defstruct make-project--makefile
  "Representes whole Makefile."
  (path :read-only)
  (targets :read-only))

(defvar make-project-column-margin 4
  "Amount of spaces during `completing-read' between Makefile target info.")

(defvar make-project--treesit-qeury
  '(((comment)
     :? @comment
     :anchor
     (rule
      (targets (word) @target)
      normal:
      (prerequisites (word))
      :? @prerequisites)))
  "Tree-sitter query to construct `make-project--makefile-target' from.")

(defvar make-project--target-column-width)
(defvar make-project--prerequisites-column-width)

;;;###autoload
(defun make-project-run ()
  "Select and run project's Makefile target."
  (interactive)
  (treesit-parser-create 'make)
  (let*
      ((default-directory (project-root (project-current)))
       (makefiles (mapcar #'make-project--parse-project-makefile (make-project--find-makefiles)))
       (max-makefile-width
        (make-project--max-width-by
         'make-project--makefile-path makefiles))
       (targets-alist
        (apply #'append (mapcar (lambda (makefile)
                                   (mapcar(lambda (target)
                                            (cons (concat
                                                   (make-project--makefile-path makefile)
                                                   " / "
                                                   (make-project--makefile-target-name target))
                                                  (cons
                                                   (make-project--makefile-path makefile)
                                                   (make-project--makefile-target-name target))))
                                          (make-project--makefile-targets makefile)))
                                 makefiles)))
       (selected (completing-read "Make target: " targets-alist)))
    (let* ((makefile2target (alist-get selected targets-alist nil nil #'string=))
           (makefile (car makefile2target))
           (target (cdr makefile2target))
           (default-directory
            (file-name-directory (expand-file-name makefile))))
           (compile (format "make %s" target)))))

(defun make-project--select-makefile ()
  "Searches for the all Makefile's in the `default-directory'.
Returns path if only one Makefile was found.
Else interactively requests selection."
  (message "DEFAULT DIRECTORY %s" default-directory)
  (let ((files
         (make-project--find-makefiles)))
    (if (eq (length files) 1)
        (car files)
      (completing-read "Makefile: " files))))

(defun make-project--find-makefiles ()
  "Finds all make files starting from default directory."
  (mapcar
   (lambda (path) (string-replace default-directory "" path))
   (directory-files-recursively
    default-directory "^Makefile$"
    nil (lambda (x) (not (string-match-p "/\\." x))))))

(defun make-project--parse-project-makefile (makefile)
  "Returns a table of available targets in MAKEFILE."
  (if (file-exists-p makefile)
      (with-temp-buffer
        (insert-file-contents makefile)
        (make-make-project--makefile
         :path makefile
         :targets (make-project--parse-makefile)))
    (warn "No Makefile found in project root")))

(defun make-project--parse-makefile ()
  "Assumes that Makefile opened in the current buffer.
Returns a `list' of `make-project--makefile-target'."
  (treesit-parser-create 'make)
  (let* ((root (treesit-buffer-root-node))
         (target-nodes
          (treesit-query-capture root make-project--treesit-qeury))
         (targets
          (mapcar
           (lambda (el)
             (cons (car el) (treesit-node-text (cdr el))))
           target-nodes)))
    (make-project--format-treesit-nodes targets)))

(defun make-project--format-treesit-nodes (nodes)
  "Format a list of treesit NODES into a list of `make-project--makefile-target'."
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
            (push (make-make-project--makefile-target
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
            (push (make-make-project--makefile-target
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
      (push (make-make-project--makefile-target
             :name current-target
             :comment current-comment
             :prerequisites current-prerequisites)
            result))
    result))

(defun make-project--max-width-by (predicate sequence)
  "Max width of each element of SEQUENCE mapped with PREDICATE."
  (cl-reduce
   #'max
   (mapcar
    (lambda (el)
      (let ((value (funcall predicate el)))
        (if value
            (string-width value)
          0)))
    sequence)))

(defun make-project--calculate-padding (max-width &optional value)
  "Diff of MAX-WIDTH and VALUE."
  (- max-width
     (if value
         (string-width value)
       0)))

(provide 'make-project)
;;; make-project.el ends here
