;;; scimotions --- Extended movements -*- lexical-binding: t; -*-

;;; Commentary:
;; Several motions that helps to select and edit text

;;; Code:
(defun scimotions-region-inner (left-symbol &optional right-symbol)
  "Selects region inside sexp of LEFT-SYMBOL and RIGHT-SYMBOL.
If RIGHT-SYMBOL is nil then used LEFT-SYMBOL"
  (if (search-backward left-symbol
                       (save-excursion (backward-sentence))
                       t)
      (forward-char)
    (search-forward left-symbol))
  (set-mark (point))
  (search-forward (or right-symbol left-symbol))
  (backward-char))

(defvar scimotions-keymap nil
  "SciMotions keymap.")
(define-prefix-command 'scimotions-keymap)
(keymap-set global-map "C-;" 'scimotions-keymap)

(defvar scimotions--symbol-to-sexp
  '(("\"" ("\""))
    ("'" ("'"))
    ("(" ("(" ")"))
    ("[" ("[" "]"))
    ("{" ("{" "}"))))

(dolist (item scimotions--symbol-to-sexp)
  (keymap-set
   scimotions-keymap (car item)
   (lambda ()
     (interactive)
     (apply #'scimotions-region-inner (car (cdr item))))))

(provide 'scimotions)
;;; scimotions.el ends here
