(setq vc-follow-symlinks t)
(org-babel-load-file
 (expand-file-name "config.org" user-emacs-directory))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-hide ((t nil))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(enwc-wired-device "wlan0")
 '(enwc-wireless-device "wlan0")
 '(org-agenda-files nil)
 '(package-selected-packages '(make-project))
 '(package-vc-selected-packages
   '((make-project :url "https://github.com/scipunch/make-project"
                   :branch "main")))
 '(warning-suppress-types '((comp))))
(put 'downcase-region 'disabled nil)
