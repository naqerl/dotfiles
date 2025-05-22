;;; Compiled snippets and support files for `makefile-gmake-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'makefile-gmake-mode
		     '(("deploy"
			"deploy-$1: install-collections\n	ansible-playbook $1.yml\n"
			"deploy" nil nil
			((yas-indent-line nil)
			 (yas-wrap-around-region 'nil))
			"/home/user/.emacs.d/snippets/makefile-gmake-mode/deploy"
			nil nil)))


;;; Do not edit! File generated at Thu May 22 16:23:52 2025
