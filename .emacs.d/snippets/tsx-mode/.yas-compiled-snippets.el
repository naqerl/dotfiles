;;; Compiled snippets and support files for `tsx-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'tsx-mode
		     '(("fcwp"
			"export type `fc-name`Props = {\n\n}\n\nexport const `fc-name`: React.FC<`fc-name`Props> = ({}) => {\n  return (\n    <${1:}>\n      $0\n    </$1>\n  )\n}"
			"functional-component-with-props" nil nil
			((yas-wrap-around-region nil)
			 (fc-name
			  (file-name-sans-extension (buffer-name))))
			"/home/user/.emacs.d/snippets/tsx-mode/functional-component-with-props"
			"C-u C-c s c" nil)
		       ("fc"
			"export const `fc-name`: React.FC<`fc-name`> = () => {\n  return (\n    <${1:}>\n      $0\n    </$1>\n  )\n}"
			"functional-component" nil nil
			((yas-wrap-around-region nil)
			 (fc-name
			  (file-name-sans-extension (buffer-name))))
			"/home/user/.emacs.d/snippets/tsx-mode/functional-component"
			"C-c s c" nil)
		       ("cl" "console.log($1);$0" "console-log" nil
			nil
			((yas-wrap-around-region nil)
			 (fc-name
			  (file-name-sans-extension (buffer-name))))
			"/home/user/.emacs.d/snippets/tsx-mode/console-log"
			nil nil)))


;;; Do not edit! File generated at Thu May 22 21:23:38 2025
