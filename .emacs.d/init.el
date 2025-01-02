(org-babel-load-file
 (expand-file-name "config.org" user-emacs-directory))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(make-project rust-docs))
 '(package-vc-selected-packages '((rust-docs :url "https://github.com/scipunch/rust-docs")))
 '(warning-suppress-types '((comp))))
(put 'downcase-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-doc-face ((t (:inherit italic :foreground "#868686"))))
 '(font-lock-keyword-face ((t (:inherit italic :foreground "#BEC3C6" :weight bold))))
 '(font-lock-string-face ((t (:inherit italic :foreground "#738678"))))
 '(fringe ((t (:foreground "#BEC3C6" :background "#BEC3C6"))))
 '(org-code ((t (:inherit default :background "black" :foreground "#e6e6e6" :box (:line-width (1 . 1) :color "#868686" :style released-button)))))
 '(org-table ((t (:inherit default :foreground "#b6b6b6"))))
 '(window-divider ((t (:foreground "#424242"))))
 '(window-divider-first-pixel ((t (:foreground "#424242"))))
 '(window-divider-last-pixel ((t (:foreground "#424242")))))
