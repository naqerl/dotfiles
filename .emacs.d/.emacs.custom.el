(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ignored-local-variable-values
   '((eval progn
	   (add-to-list 'gptel-directives
			`(aishift-project-analysist \
			  , (with-temp-buffer
			      (insert-file-contents
			       (expand-file-name
				"aishift-project-analysist-prompt.org"
				(project-root
				 (project-current))))
			      (buffer-string))))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
